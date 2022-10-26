import json
from xml.etree.ElementTree import tostring

f = open('sports.json')
out = open("score.csv", "w") 
data = json.load(f)

plays = []

## scoring vars
YARDS_SCORE = 20
FG_MISS = 40
RETURN_SCORE = 40

team1_score = 0
team2_score = 0

x = 1
type = ''
yards = 0
team = ''

## TODO: Thrown out due to bad behavior, look for "Disqualification"

for i in data['items']:
    score1 = 0 ## scoring for team who has possession this play
    score2 = 0 ## scoreing for defending team this play
    PAT = []
    ## turnover = False
    plays.append((i['text'],i['type'], i['statYardage']))
    type = i['type']['id']
    yards = i['statYardage']
    
    if i['scoreValue'] == 6:
        PAT = i["pointAfterAttempt"]

    if type == '7': ## or type == '9': ## 7 is regular sack, 9 is fumble with own recovery
        score2 += 0.5
    elif type != '52' and i['scoreValue'] == 0 and i['start']['down'] == 4 and i['end']['down'] == 1 and i['start']['team'] != i['end']['team']: #TO on downs
        score2 += 0.5
    elif type == '26': ## pick
        score1 += 0.5
    elif type == '36': ## pick 6
        score1 += 1.5
        
        if PAT['value'] == 2: ## successful 2pc. id = 16 is rush, 15 is pass
            score2 += 0.5
        elif PAT['id'] == 62: ## missed xp
            score1 += 2.0
        elif PAT['value'] == 0: ## failed 2pc
            score1 += 0.5
    else:

        if type == '29': ## Fumble recovered by opponent
            if i['scoreValue'] == 6:
                score1 += 1.5 ## fumble TD
                
                if PAT['value'] == 2: ## successful 2pc. id = 16 is rush, 15 is pass
                    score1 += 0.5
                elif PAT['id'] == 62: ## missed xp
                    score2 += 2.0
                elif PAT['value'] == 0: ## failed 2pc
                    score2 += 0.5
            else:
                score1 += 0.5

        if type == '59': ## fg good TODO: missed FG under 40, also not sure if this logic is right
            score1 += 0.5
        elif type == '60' and yards < FG_MISS:
            score2 += 0.5
        elif type == '52' or type == '53': ## punt return/kickoff return
            if yards > RETURN_SCORE:
                    score1 += 0.5
        else:
            if yards >= YARDS_SCORE:
                score1 += 0.5 

            if yards <= -YARDS_SCORE:
                score2 += 0.5
            
            if i['scoreValue'] == 6: ## td
                score1 += 1.0

                if PAT['value'] == 2: ## successful 2pc. id = 16 is rush, 15 is pass
                    score1 += 0.5
                elif PAT['id'] == 62: ## missed xp
                    score2 += 2.0
                elif PAT['value'] == 0: ## failed 2pc
                    score2 += 0.5

    # if score1 > 0 or score2 > 0:
    #     line = i['text']+"\t"+str(score1)+"\t"+str(score2)+"\n"
    #     print(line)
    #     out.write(line)

    
    if i['team']['$ref'] == "http://sports.core.api.espn.com/v2/sports/football/leagues/nfl/seasons/2022/teams/17?lang=en&region=us":
        team1_score += score1
        team2_score += score2
        line = i['text']+"\tPats score "+str(score1)+"\tChi scores "+str(score2)+"\n"
        out.write(line)
    else:
        team1_score += score2
        team2_score += score1
        line = i['text']+"\tPats score "+str(score2)+"\tChi scores "+str(score1)+"\n"
        out.write(line)

line = "team1 score: "+str(team1_score)+" team2 score: "+str(team2_score)
print(line)
out.write("\n")
out.write(line)

f.close()
out.close()