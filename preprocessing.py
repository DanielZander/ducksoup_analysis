"""
Preprocessing script of behavioral data for the ultimatum game
"""
import pandas as pd
import os

# %%
currentDir = os.getcwd()
csvPath = os.path.join(currentDir, 'data.csv')
print(csvPath)
columnsInterest = ['participant.code', ]
raw_data = pd.read_csv(csvPath)

column_names = ["sid","participant_code", "round_nb", "role", "dyad", "manip",
                 "trial_payoff", "responded", "sent_amount", "offer_response", "rt","prolific_id"]
clean_data = pd.DataFrame(columns=column_names)

# %%
for subject in raw_data['participant.id_in_session']:
    subject_row = (raw_data[raw_data['participant.id_in_session'] == subject])
    
    for round in range(1, 21):
        
        if subject_row[f'ultimatum_game.{round}.player.player_role'].values[0] == "proposer":
            rt = subject_row[f'ultimatum_game.{round}.player.proposer_response_time_ms'].values[0]
        else:
            rt = subject_row[f'ultimatum_game.{round}.player.responder_response_time_ms'].values[0]
            
        row_data = {
            'participant_code': subject_row['participant.code'].values[0],
            'prolific_id': subject_row['ultimatum_game.20.player.prolific_id'].values[0],
            'sid': subject_row['session.config.name'].values[0],
            'trial_payoff': subject_row[f'ultimatum_game.{round}.player.payoff'].values[0],
            'round_nb': subject_row[f'ultimatum_game.{round}.player.round_nb'].values[0],
            'role': subject_row[f'ultimatum_game.{round}.player.player_role'].values[0],
            'is_dom': subject_row[f'ultimatum_game.{round}.player.is_dom'].values[0],
            'dyad': subject_row[f'ultimatum_game.{round}.player.dyad'].values[0],
            'manip': subject_row[f'ultimatum_game.{round}.player.is_dom'].values[0],
            'responded': subject_row[f'ultimatum_game.{round}.player.responded'].values[0],
            'sent_amount': subject_row[f'ultimatum_game.{round}.group.sent_amount'].values[0],
            'offer_response': subject_row[f'ultimatum_game.{round}.group.responder_accepted'].values[0],
            'rt': rt
            }   
        
        clean_data = pd.concat([clean_data, pd.DataFrame([row_data])], ignore_index=True)
        
# %%

clean_data.to_csv('clean_data.csv', index=False)
          