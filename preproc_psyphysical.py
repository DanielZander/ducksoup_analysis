"""
Preprocessing script of behavioral data for the Psychophysical game
"""
import pandas as pd
import os
from statistics import mean

# %%

data_name = "psyphysical_main1.csv"

script_location = os.path.dirname(os.path.realpath(__file__))
csvPath = os.path.join(script_location, 'raw_data/Psychophysical/' + f'{data_name}')
print(csvPath)
raw_data = pd.read_csv(csvPath)

column_names = ["sid","session_code", "mk_session","prolific_id","participant_code", "round_nb", "role","player", "dyad", "manipulation", "mean_social_dominance","mean_aggresive_dominance"]
clean_data = pd.DataFrame(columns=column_names)

column_debrief = ["sid","session_code", "mk_session", "prolific_id", "participant_code", "player", "mean_social_dominance", "mean_aggresive_dominance", 
                  "sound_quality", "sound_comment", "fidelity", "fidelity_comment", "xp_goal", "enough_time", "manipulation",
                  'detection_degree', "manipulation_comment", "unique_interactions", "xp_feedback"]
clean_data_debrief = pd.DataFrame(columns=column_debrief)

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
            'session_code': subject_row['session.code'].values[0],
            'prolific_id': subject_row['ultimatum_game.20.player.prolific_id'].values[0],
            'sid': subject_row['session.config.name'].values[0],
            'mk_session': subject_row['session.config.id'].values[0],
            'trial_payoff': subject_row[f'ultimatum_game.{round}.player.trial_payoff'].values[0],
            'round_nb': subject_row[f'ultimatum_game.{round}.player.round_nb'].values[0],  
            'role': subject_row[f'ultimatum_game.{round}.player.player_role'].values[0],
            'player': subject_row['participant.id_in_session'].values[0],
            'dyad': subject_row[f'ultimatum_game.{round}.player.dyad'].values[0],
            'manipulation': subject_row[f'ultimatum_game.{round}.player.manipulation'].values[0],
            'responded': subject_row[f'ultimatum_game.{round}.player.responded'].values[0],
            'sent_amount': subject_row[f'ultimatum_game.{round}.group.sent_amount'].values[0],
            'offer_response': subject_row[f'ultimatum_game.{round}.group.responder_accepted'].values[0],
            'rt': rt,
            'mean_social_dominance': mean([float(subject_row[f'ultimatum_game.20.player.social_dominance_{question_social}'].values[0]) for question_social in range(1, 9)]),
            'mean_aggresive_dominance': mean([float(subject_row[f'ultimatum_game.20.player.aggressive_dominance_{question_aggresive}'].values[0]) for question_aggresive in range(1, 8)])
            }   
        
        
        clean_data = pd.concat([clean_data, pd.DataFrame([row_data])], ignore_index=True)
        

    row_data_debrief = {
        'participant_code': subject_row['participant.code'].values[0],
        'session_code': subject_row['session.code'].values[0],
        'prolific_id': subject_row['ultimatum_game.20.player.prolific_id'].values[0],
        'sid': subject_row['session.config.name'].values[0],
        'mk_session': subject_row['session.config.id'].values[0],
        'player': subject_row['participant.id_in_session'].values[0],
        'mean_social_dominance': mean([float(subject_row[f'ultimatum_game.20.player.social_dominance_{question_social}'].values[0]) for question_social in range(1, 9)]),
        'mean_aggresive_dominance': mean([float(subject_row[f'ultimatum_game.20.player.aggressive_dominance_{question_aggresive}'].values[0]) for question_aggresive in range(1, 8)]),
        'sound_quality': subject_row['ultimatum_game.20.player.final_quality'].values[0],
        'sound_comment': subject_row['ultimatum_game.20.player.final_quality_comment'].values[0],
        'fidelity':  subject_row['ultimatum_game.20.player.final_conversation_fidelity'].values[0],
        'fidelity_comment': subject_row['ultimatum_game.20.player.final_conversation_fidelity_comment'].values[0],
        'xp_goal': subject_row['ultimatum_game.20.player.final_xp_goal'].values[0],
        'enough_time': subject_row['ultimatum_game.20.player.enough_time'].values[0],
        'manipulation': subject_row['ultimatum_game.20.player.manip_yes_no'].values[0],
        'detection_degree': subject_row['ultimatum_game.20.player.detection_degree'].values[0],
        'manipulation_comment': subject_row['ultimatum_game.20.player.final_manipulation_comment'].values[0],
        'unique_interactions': subject_row['ultimatum_game.20.player.unique_interactions'].values[0],
        'xp_feedback':  subject_row['ultimatum_game.20.player.game_feedback'].values[0],
        
        
        }
        
    clean_data_debrief = pd.concat([clean_data_debrief, pd.DataFrame([row_data_debrief])], ignore_index=True)

clean_data.to_csv(f'clean_data\clean_{data_name}.csv', index=False)
clean_data_debrief.to_csv(f'clean_data\clean_{data_name}_debrief.csv', index=False)

# %% COMBINE CLEAN 

df1 = pd.read_csv('clean_data\clean_data_p4;1_dogizqxo.csv')
df2 = pd.read_csv('clean_data\clean_data_p4;2_pl6dy4ai.csv')
combined_df = pd.concat([df1, df2])
combined_df.to_csv('clean_data\combined_data\combined_data_pilot4.csv', index=False)

"""
df1 = pd.read_csv('clean_data\clean_data_mk1_i5pyilec.csv')
df2 = pd.read_csv('clean_data\clean_data_mk2_dzoz67y9.csv')
df3 = pd.read_csv('clean_data\clean_data_mk3_mklqs5l0.csv')
df4 = pd.read_csv('clean_data\clean_data_mk4_080ps8pg.csv')

df1_deb = pd.read_csv('clean_data\clean_data_mk1_i5pyilec_debrief.csv')
df2_deb = pd.read_csv('clean_data\clean_data_mk2_dzoz67y9_debrief.csv')
df3_deb = pd.read_csv('clean_data\clean_data_mk3_mklqs5l0_debrief.csv')
df4_deb = pd.read_csv('clean_data\clean_data_mk4_080ps8pg_debrief.csv')

combined_df = pd.concat([df1, df2, df3, df4])
combined_df_debrief = pd.concat([df1_deb, df2_deb, df3_deb, df4_deb])

combined_df.to_csv('clean_data\combined_data\combined_data.csv', index=False)
combined_df_debrief.to_csv('clean_data\combined_data\combined_data_debrief.csv', index=False)
"""









          
