import spotipy
import csv
from spotipy.oauth2 import SpotifyClientCredentials

client_credentials_manager = SpotifyClientCredentials(client_id="5fe1d0b6b28645298bbc05a057137ee2", client_secret='6f2e3814cb2a4cb680af48d6be2a78fc')
sp = spotipy.Spotify(client_credentials_manager=client_credentials_manager)

def readFromCsvImport(csv_file, column_name):
	df = pd.read_csv(csv_file)
	saved_column = df[column_name] #you can also use df['column_name']
	return list(saved_column)


def popularityFromURI(uriList):
	trackList = sp.tracks(uriList)["tracks"]
	print trackList
	return [i["popularity"] for i in trackList]

def getCategories():
	r = sp.categories(country=None, locale=None, limit=50, offset=0)["categories"]["items"] # change this
	return [i["id"] for i in r]


songs = {}
def getSongsByCategories(category):
	try:
		response = sp.category_playlists(category_id = category, limit = 50)["playlists"]["items"] # change limit
	except:
		return "error occurred"
	playlistName = [i["name"] for i in response]
	playlistsURI = [i["uri"] for i in response]
	print(playlistName)

	prevLen = len(songs)
	
	for i in playlistsURI:
		inter = playlist(playlist_id= i)["tracks"]["items"]
		# print(inter)
		print(len(inter))
		for track in inter:
			j = track.get("track", None)
			if j and j["popularity"]>2:
				songs[j["id"]] = {"popularity":j["popularity"], "name": j["name"]}

	return str(len(songs)- prevLen) + "songs added"


def playlist(playlist_id, fields=None, market=None):
    """ Gets playlist by id
        
        Parameters:
        - playlist - the id of the playlist
        - fields - which fields to return
        - market - An ISO 3166-1 alpha-2 country code or the string from_token.
        """
    plid = sp._get_id('playlist', playlist_id)
    return sp._get("playlists/%s" % (plid), fields=fields)


def generateData():
	# c = ['toplists', 'pop', 'hiphop', 'workout',  'country',  'latin', 'chill', u'edm_dance', u'rnb', u'rock',  u'party',  u'holidays', u'amplify_latinx', 'jazz',  'gaming', 'romance',  'kpop', 'popculture', 'metal', u'reggae', u'soul', u'blues', u'punk', u'funk',  u'travel', u'family']

	c = ['pop']
	for c0 in c:
		print(c0)
		getSongsByCategories(c0)

	ids = songs.keys()

	l = len(ids)
	print "songs length is "+ str(l)
	for i in range(0, l, 50):
		# get the audio features for the 50 songs
		f = sp.audio_features(tracks = ids[i: min(i+50, l)])

		for s in f:
			if s and songs.get(s["id"], None):

				songs[s["id"]].update(s)
				# print(songs[s["id"]].keys())

def writeFile(d):
	col = ['liveness', 'tempo', 'energy', 'analysis_url', 'speechiness', 'mode', 'instrumentalness', 'key', 'duration_ms', 'id', 'name', 'popularity', 'uri', 'acousticness', 'track_href', 'time_signature', 'loudness', 'valence', 'type', 'danceability']
	csv_file = "SpotifyPopSongsPopCategory.csv"
	try:
		with open(csv_file, 'w') as csvfile:
			writer = csv.DictWriter(csvfile, fieldnames=col)
			writer.writeheader()
			for data in d.values():
				try:
					writer.writerow(data)
				except UnicodeEncodeError:
					print("UnicodeEncodeError")
	except IOError:
		print("I/O error")

# print(getCategories())
generateData()
print(len(songs))
writeFile(songs)


# # inter = playlist(playlist_id= '6zeeWid2sgw4lap2jV61PZ')["tracks"]["items"]
# # songs = {}
# # songs['6zeeWid2sgw4lap2jV61PZ'].update(sp.audio_features(tracks=["6zeeWid2sgw4lap2jV61PZ"])[0])
# # print songs
# # print(getSongsByCategories("toplists"))
# # response = sp.category_playlists(category_id = "toplists", limit = 50)["playlists"]["items"]
# # print response[0]

# print(playlist(playlist_id= '37i9dQZF1DXcBWIGoYBM5M')["tracks"]["items"])
# # print(sp.audio_features(tracks=["6zeeWid2sgw4lap2jV61PZ"]))
# # generateData()
# # print(songs)

