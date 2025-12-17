import subprocess
import sys

# Replace 'package_name' with the package you want to install
package_name = "REData"

subprocess.check_call([sys.executable, "-m", "pip", "install", package_name])

from REData import REData

category = 'demanda'
widget = 'evolucion'

start_date = '2013-01-01T00:00'
end_date = '2013-01-12T00:00'
time_trunc = 'day'

RED_stream = REData(category, widget)
df = RED_stream.query_REData(start_date, end_date, time_trunc)

df.head()


