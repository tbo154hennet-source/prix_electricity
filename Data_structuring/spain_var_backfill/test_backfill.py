"""Offline unit tests against the published price sample and REData-shaped JSON fixtures."""
import json
import tempfile
import unittest
from pathlib import Path

from backfill_spain_macro import (
    read_price_csv, extract_demand, extract_generation, infer_monthly_gwh,
    make_rows, ree_url, main,
)

SAMPLE_PRICE = 'mes,es_medio\n01/2010,29.08\n02/2010,27.68\n01/2014,33.62\n'


def fixture(series):
    return {"included": [
        {"attributes": {"title": label, "values": [
            {"datetime": f"2010-{month:02d}-01T00:00:00+01:00", "value": val}
            for month, val in enumerate(values, 1)]}}
        for label, values in series.items()
    ]}


class BackfillTests(unittest.TestCase):
    def test_price_correct_country_and_month(self):
        p = read_price_csv('\ufeff' + SAMPLE_PRICE)
        self.assertEqual(p['2010-01'], 29.08)
        self.assertEqual(p['2014-01'], 33.62)

    def test_ree_url_peninsular(self):
        u = ree_url('demanda', 'evolucion', 2010)
        self.assertIn('geo_ids=8741', u)
        self.assertIn('time_trunc=month', u)
        self.assertIn('2010-01-01', u)

    def test_demand_select_actual_and_mwh_convert(self):
        data = fixture({'Demanda': [20_000_000 + m * 1000 for m in range(12)],
                        'Demanda prevista': [30_000_000] * 12})
        x, prov = extract_demand(data)
        self.assertAlmostEqual(x['2010-01'], 20_000)
        self.assertEqual(len(x), 12)

    def test_generation_sum_technologies_excluding_aggregate(self):
        data = fixture({'Hidráulica': [8_000_000] * 12,
                        'Nuclear': [11_000_000] * 12,
                        'Eólica': [5_000_000] * 12,
                        'Generación total': [24_000_000] * 12})
        g, provenance = extract_generation(data)
        self.assertEqual(g['2010-01'], 24000)
        self.assertEqual(len(g), 12)

    def test_price_zero_distinguished_from_missing(self):
        p = read_price_csv('mes,es_medio\n01/2010,0\n')
        self.assertEqual(p['2010-01'], 0)
        with self.assertRaises(ValueError):
            make_rows(p, None, None, 2010, 2010, price_only=True)

    def test_reject_incomplete_ree_calendar(self):
        prices = {f'2010-{m:02d}': 40 for m in range(1, 13)}
        loads = {f'2010-{m:02d}': 20000 for m in range(1, 12)}
        gens = {f'2010-{m:02d}': 22000 for m in range(1, 13)}
        with self.assertRaisesRegex(ValueError, 'Missing REE'):
            make_rows(prices, loads, gens, 2010, 2010)

    def test_zero_padding_marked_unobserved(self):
        prices = {f'2010-{m:02d}': 40 for m in range(1, 13)}
        x = {f'2010-{m:02d}': 20000 for m in range(1, 13)}
        rows = make_rows(prices, x, x, 2010, 2010)
        self.assertEqual(len(rows), 12)
        self.assertTrue(all(r['proxy_supply'] == r['proxy_demand'] == 0 and r['proxy_observed'] == 0 for r in rows))


    def test_cli_full_ree_with_offline_json_fixtures(self):
        # Full CLI, data acquisition substituted with REE-shaped local JSON.
        # Numbers are SYNTHETIC: this test checks IO/parsing/calendar only.
        with tempfile.TemporaryDirectory() as d:
            folder = Path(d)
            price_path = folder / 'prices.csv'
            price_path.write_text('mes,es_medio\n' + ''.join(
                f'{m:02d}/{y},{20 + m / 10:.2f}\n'
                for y in range(2010, 2015) for m in range(1, 13)))
            for year in range(2010, 2015):
                demand = {'Demanda': [20_000_000 + year * 100 + m * 10 for m in range(12)]}
                generation = {
                    'Nuclear': [12_000_000] * 12,
                    'Hidráulica': [6_000_000] * 12,
                    'Eólica': [7_000_000] * 12,
                }
                for category, series in [('load', demand), ('generation', generation)]:
                    payload = { 'included': [
                        {'attributes': {'title': name, 'values': [
                           {'datetime': f'{year}-{m:02d}-01T00:00:00+01:00', 'value': value}
                           for m, value in enumerate(annual_values, 1)]}}
                        for name, annual_values in series.items()
                    ]}
                    (folder / f'{category}_{year}.json').write_text(json.dumps(payload))
            out = folder / 'monthly.csv'
            status = main(['--price-file', str(price_path), '--ree-json-dir', str(folder),
                           '--output', str(out), '--start-year', '2010', '--end-year', '2014'])
            self.assertEqual(status, 0)
            with out.open(newline='') as inp:
                rows = list(__import__('csv').DictReader(inp))
            self.assertEqual(len(rows), 60)
            self.assertAlmostEqual(float(rows[0]['load_gwh']), 20201.0)
            self.assertEqual(float(rows[0]['gen_tot_gwh']), 25000.0)
            self.assertEqual(rows[-1]['date'], '2014-12-01')
            self.assertEqual(rows[0]['proxy_observed'], '0')

    def test_cli_offline_price_full_2010_to_2014(self):
        # End-to-end test with a local CSV in the actual public mirror schema.
        with tempfile.TemporaryDirectory() as d:
            price = Path(d) / 'prices.csv'
            price.write_text('mes,es_medio\n' + ''.join(f'{m:02d}/{y},32.5\n' for y in range(2010,2015) for m in range(1,13)))
            output = Path(d) / 'backfill.csv'
            self.assertEqual(main(['--price-file', str(price), '--price-only', '--output', str(output)]), 0)
            self.assertEqual(len(output.read_text().splitlines()), 61)


if __name__ == '__main__':
    unittest.main(verbosity=2)
