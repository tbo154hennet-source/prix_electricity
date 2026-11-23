from io import BytesIO
from pathlib import Path
import pandas as pd
from download_spain_price_monthly import parse_mirror_csv, parse_ree_report_text, load_ree_offline, build_output
p = Path(__file__).parent
ree = load_ree_offline(p / 'ree_monthly_weighted_2007_2009.csv')
assert len(ree) == 36
assert ree.loc[ree.date.eq(pd.Timestamp('2007-01-01')), 'price_ree_weighted_eur_mwh'].iloc[0] == 47.20
mirror = parse_mirror_csv((p / 'omie_es_price_2010_2014.csv').read_bytes())
assert len(mirror) == 60
out = build_output(ree,mirror,2007,2014,combine=True)
assert len(out) == 96 and (out.price_source != 'missing').all()
assert sum(out.price_source == 'REE_weighted') == 36
assert sum(out.price_source == 'OMIE_arithmetic_mirror') == 60
fixture='Sistema peninsular. Energía y precios medios ponderados en el mercado diario Energía (*) GWh Precio (€/MWh) Mínimo horario Medio mensual Máximo horario '
months=['Enero','Febrero','Marzo','Abril','Mayo','Junio','Julio','Agosto','Septiembre','Octubre','Noviembre','Diciembre']
fixture+=' '.join(f'{month} 19.831 29,50 58,88 95,00' for month in months)+' Total 222.158 10,00 65,30 103,15 (*) Incluye bombeo.'
assert len(parse_ree_report_text(fixture,2008)) == 12
out.to_csv(p/'spain_price_2007_2014_exploratory_MIXED.csv',index=False,date_format='%Y-%m-%d')
print('Tests OK: REE 36 months, OMIE 60 months, 96 contiguous months, parser REE fixture.')
