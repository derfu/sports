import json
from xml.etree.ElementTree import tostring

f = open('sports.json')
out = open("score.csv", "w") 
data = json.load(f)

plays = []

## scoring vars
YARDS_SCORE = 20

team1_score = 0
team2_score = 0

x = 1
type = ''
yards = 0
team = ''

for i in data['items']:
    score1 = 0
    score2 = 0
    PAT = []
    turnover = False
    plays.append((i['text'],i['type'], i['statYardage']))
    type = i['type']
    yards = i['statYardage']
    
    if i['scoreValue'] == 6:
        PAT = i["pointAfterAttempt"]

    if type['id'] == 7 or type['id'] == 9: ## 7 is regular sack, 9 is sack fumble with own recovery
        score2 += 0.5
    elif type['id'] == 26: ## pick
        score2 += 0.5
    elif type['id'] == 36: ## pick 6
        score2 += 1.5
        
        if PAT['value'] == 2: ## successful 2pc. id = 16 is rush, 15 is pass
            score2 += 0.5
        elif PAT['id'] == 62: ## missed xp
            score1 += 2.0
        elif PAT['value'] == 0: ## failed 2pc
            score1 += 0.5
    else:

        if type['id'] == 29: ## Fumble recovered by opponent
            if i['scoreValue'] == 6:
                score2 += 1.5 ## fumble TD
                
                if PAT['value'] == 2: ## successful 2pc. id = 16 is rush, 15 is pass
                    score2 += 0.5
                elif PAT['id'] == 62: ## missed xp
                    score1 += 2.0
                elif PAT['value'] == 0: ## failed 2pc
                    score1 += 0.5
            else:
                score2 += 0.5

        if yards > YARDS_SCORE:
            score1 += 0.5 

        if i['scoreValue'] == 3: ## fg
            score1 += 0.5
        elif i['scoreValue'] == 6: ## td
            score1 += 1.0

            if PAT['value'] == 2: ## successful 2pc. id = 16 is rush, 15 is pass
                score1 += 0.5
            elif PAT['id'] == 62: ## missed xp
                score2 += 2.0
            elif PAT['value'] == 0: ## failed 2pc
                score2 += 0.5
    if score1 > 0 or score2 > 0:
        line = i['text']+"\t"+str(score1)+"\t"+str(score2)+"\n"
        print(line)
        out.write(line)

    
    if i['team']['$ref'] == "http://sports.core.api.espn.com/v2/sports/football/leagues/nfl/seasons/2022/teams/17?lang=en&region=us":
        team1_score += score1
        team2_score += score2
    else:
        team1_score += score2
        team2_score += score1
line = "team1 score: "+str(team1_score)+" team2 score: "+str(team2_score)
print(line)
out.write("\n")
out.write(line)

f.close()
out.close()