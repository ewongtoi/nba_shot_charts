# largely borrowing from 
# https://datavizardry.com/2020/01/28/nba-shot-charts-part-1/

from nba_api.stats.endpoints import shotchartdetail
import simplejson as json
import pandas as pd
import requests


# response = shotchartdetail.ShotChartDetail(
# 	team_id=0,
# 	player_id=201935,
# 	season_nullable='2018-19',
# 	season_type_all_star='Regular Season'
# )
# 
# content = json.loads(response.get_json())






# 
# response = shotchartdetail.ShotChartDetail(
# 	team_id=0,
# 	player_id=0,
# 	season_nullable='2018-19',
# 	season_type_all_star='Playoffs'
# )
# 
# content = json.loads(response.get_json())
# 
# 
# # transform contents into dataframe
# results = content['resultSets'][0]
# headers = results['headers']
# rows = results['rowSet']
# df = pd.DataFrame(rows)
# df.columns = headers
# df.shape




url_base = 'https://stats.nba.com/stats/shotchartdetail'

headers = {
		'Host': 'stats.nba.com',
		'Connection': 'keep-alive',
		'Accept': 'application/json, text/plain, */*',
		'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36',
		'Referer': 'https://stats.nba.com/',
		"x-nba-stats-origin": "stats",
		"x-nba-stats-token": "true",
		'Accept-Encoding': 'gzip, deflate, br',
		'Accept-Language': 'en-US,en;q=0.9',
	}

parameters = {
	'ContextMeasure': 'FGA',
	'LastNGames': 0,
	'LeagueID': '00',
	'Month': 0,
	'OpponentTeamID': 0,
	'Period': 0,
	'PlayerID': 0,
	'SeasonType': 'Regular Season',
	'TeamID': 0,
	'VsDivision': '',
	'VsConference': '',
	'SeasonSegment': '',
	'Season': '2018-19',
	'RookieYear': '',
	'PlayerPosition': '',
	'Outcome': '',
	'Location': '',
	'GameSegment': '',
	'GameId': '',
	'DateTo': '',
	'DateFrom': ''
}


parameters2 = {
	'ContextMeasure': 'FGA',
	'LastNGames': 0,
	'LeagueID': '00',
	'Month': 0,
	'OpponentTeamID': 0,
	'Period': 0,
	'PlayerID': 0,
	'SeasonType': 'Playoffs',
	'TeamID': 0,
	'VsDivision': '',
	'VsConference': '',
	'SeasonSegment': '',
	'Season': '2018-19',
	'RookieYear': '',
	'PlayerPosition': '',
	'Outcome': '',
	'Location': '',
	'GameSegment': '',
	'GameId': '',
	'DateTo': '',
	'DateFrom': ''
}

response = requests.get(url_base, params=parameters, headers=headers)
content = json.loads(response.content)


# transform contents into dataframe
results = content['resultSets'][0]
headers = results['headers']
rows = results['rowSet']
df = pd.DataFrame(rows)
df.columns = headers
df.shape

# write to csv file
df.to_csv('C:/Users/ewong/Documents/GitHub/nba_shot_charts/nba_shotchartdetail_2018-191.csv', index=False)

response2 = requests.get(url_base, params=parameters2, headers=headers)
content2 = json.loads(response2.content)


