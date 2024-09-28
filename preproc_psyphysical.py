"""
Preprocessing script of behavioral data for the Psychophysical game
"""
import pandas as pd
import os
from statistics import mean

# %%

data_name = "psyphysical_main3"

script_location = os.path.dirname(os.path.realpath(__file__))
csvPath = os.path.join(script_location, 'raw_data/Psychophysical/' + f'{data_name}' + ".csv")
print(csvPath)
raw_data = pd.read_csv(csvPath)

column_names = ["sid","session_code", "mk_session","prolific_id","participant_code", "round_nb","player", "dyad", "manipulation", "condition", "difficulty", "confidence_1",
                "confidence_2", "indv_response", "correct_response", "indv_responded_correctly", "rt_individual", "indv_stim_order", "group_choice", "grp_responded_correctly",
                "decision_count","group_rt", "both_agree", "mean_social_dominance","mean_aggresive_dominance"]
clean_data = pd.DataFrame(columns=column_names)

column_debrief = ["sid","session_code", "mk_session", "prolific_id", "participant_code", "player", "mean_social_dominance", "mean_aggresive_dominance", 
                  "sound_quality", "sound_comment", "fidelity", "fidelity_comment", "xp_goal", "manipulation",
                  'detection_degree', "manipulation_comment", "unique_interactions"]
clean_data_debrief = pd.DataFrame(columns=column_debrief)

# %%

for subject in raw_data['participant.id_in_session']:
    subject_row = (raw_data[raw_data['participant.id_in_session'] == subject])
    
    for round in range(1, 35):
        

        row_data = {
            'sid': subject_row['session.config.name'].values[0],
            'session_code': subject_row['session.code'].values[0],
            'mk_session': subject_row['session.config.id'].values[0],
            'prolific_id': subject_row['interactive_psychophysics.34.player.prolific_id'].values[0],
            'participant_code': subject_row['participant.code'].values[0],
            'round_nb': subject_row[f'interactive_psychophysics.{round}.player.round_nb'].values[0],
            'player': subject_row['participant.id_in_session'].values[0],
            'dyad': subject_row[f'interactive_psychophysics.{round}.player.dyad'].values[0],
            'manipulation':subject_row[f'interactive_psychophysics.{round}.player.manipulation'].values[0],
            'condition': subject_row[f'interactive_psychophysics.{round}.player.condition'].values[0],
            'difficulty': subject_row[f'interactive_psychophysics.{round}.player.difficulty'].values[0],
            'confidence_1': subject_row[f'interactive_psychophysics.{round}.player.individual_confidence_1'].values[0],
            'confidence_2': subject_row[f'interactive_psychophysics.{round}.player.individual_confidence_2'].values[0],
            'indv_response': subject_row[f'interactive_psychophysics.{round}.player.individual_decision'].values[0],
            'correct_response': subject_row[f'interactive_psychophysics.{round}.player.correct_response'].values[0],
            'indv_responded_correctly': subject_row[f'interactive_psychophysics.{round}.player.ind_responded_correctly'].values[0],
            'rt_individual': subject_row[f'interactive_psychophysics.{round}.player.rt_indvidual'].values[0],
            'indv_stim_order': subject_row[f'interactive_psychophysics.{round}.player.indv_stim_order'].values[0],
            'group_choice': subject_row[f'interactive_psychophysics.{round}.player.group_choice'].values[0],
            'grp_responded_correctly': subject_row[f'interactive_psychophysics.{round}.player.grp_responded_correctly'].values[0],
            'decision_count': subject_row[f'interactive_psychophysics.{round}.player.decision_count'].values[0],
            'group_rt': subject_row[f'interactive_psychophysics.{round}.group.group_rt'].values[0],
            'both_agree': subject_row[f'interactive_psychophysics.{round}.group.both_agree'].values[0],
            'mean_social_dominance': mean([float(subject_row[f'interactive_psychophysics.34.player.social_dominance_{question_social}'].values[0]) for question_social in range(1, 9)]),
            'mean_aggresive_dominance': mean([float(subject_row[f'interactive_psychophysics.34.player.aggressive_dominance_{question_aggresive}'].values[0]) for question_aggresive in range(1, 8)])
            }   
        
        
        clean_data = pd.concat([clean_data, pd.DataFrame([row_data])], ignore_index=True)
        

    row_data_debrief = {
        'participant_code': subject_row['participant.code'].values[0],
        'session_code': subject_row['session.code'].values[0],
        'prolific_id': subject_row['interactive_psychophysics.34.player.prolific_id'].values[0],
        'sid': subject_row['session.config.name'].values[0],
        'mk_session': subject_row['session.config.id'].values[0],
        'player': subject_row['participant.id_in_session'].values[0],
        'mean_social_dominance': mean([float(subject_row[f'interactive_psychophysics.34.player.social_dominance_{question_social}'].values[0]) for question_social in range(1, 9)]),
        'mean_aggresive_dominance': mean([float(subject_row[f'interactive_psychophysics.34.player.aggressive_dominance_{question_aggresive}'].values[0]) for question_aggresive in range(1, 8)]),
        'sound_quality': subject_row['interactive_psychophysics.34.player.final_quality'].values[0],
        'sound_comment': subject_row['interactive_psychophysics.34.player.final_quality_comment'].values[0],
        'fidelity':  subject_row['interactive_psychophysics.34.player.final_conversation_fidelity'].values[0],
        'fidelity_comment': subject_row['interactive_psychophysics.34.player.final_conversation_fidelity_comment'].values[0],
        'xp_goal': subject_row['interactive_psychophysics.34.player.final_xp_goal'].values[0],
        'manipulation': subject_row['interactive_psychophysics.34.player.manip_yes_no'].values[0],
        'detection_degree': subject_row['interactive_psychophysics.34.player.detection_degree'].values[0],
        'manipulation_comment': subject_row['interactive_psychophysics.34.player.final_manipulation_comment'].values[0],
        'unique_interactions': subject_row['interactive_psychophysics.34.player.unique_interactions'].values[0],
        
        
        
        }
        
    clean_data_debrief = pd.concat([clean_data_debrief, pd.DataFrame([row_data_debrief])], ignore_index=True)

# Save to a directory where you have write permissions
output_path = os.path.join(script_location, 'cleaned_data', f'{data_name}_cleaned.csv')

# Ensure the directory exists
os.makedirs(os.path.dirname(output_path), exist_ok=True)

# Save to the specified file path
clean_data.to_csv(output_path, index=False)

"""
output_directory = os.path.join(os.path.expanduser("~"), "Documents", "clean_data")
os.makedirs(output_directory, exist_ok=True)

clean_data_path = os.path.join(output_directory, f'clean_{data_name}.csv')
clean_data_debrief_path = os.path.join(output_directory, f'clean_{data_name}_debrief.csv')

clean_data.to_csv(clean_data_path, index=False)
clean_data_debrief.to_csv(clean_data_debrief_path, index=False)

print(f"Data saved to {clean_data_path}")
print(f"Debrief data saved to {clean_data_debrief_path}")

"""

# %% COMBINE CLEAN 
"""
df1 = pd.read_csv('clean_data\clean_data_p4;1_dogizqxo.csv')
df2 = pd.read_csv('clean_data\clean_data_p4;2_pl6dy4ai.csv')
combined_df = pd.concat([df1, df2])
combined_df.to_csv('clean_data\combined_data\combined_data_pilot4.csv', index=False)


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









          
