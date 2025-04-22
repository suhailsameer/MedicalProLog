% --- Dynamic predicates ---
:- dynamic symptom/2.
:- dynamic current_patient/1.

% --------------------------
% Weighted Symptoms per Disease
% --------------------------

% Format: disease_symptom(Disease, Symptom, Weight)
disease_symptom(headaches, headache, 5).

disease_symptom(cold, cough, 4).
disease_symptom(cold, sore_throat, 4).
disease_symptom(cold, fever, 2).

disease_symptom(flu, fever, 3).
disease_symptom(flu, phlegm, 3).
disease_symptom(flu, body_pains, 4).

disease_symptom(measles, fever, 3).
disease_symptom(measles, running_nose, 2).
disease_symptom(measles, watery_eyes, 2).
disease_symptom(measles, small_white_spots_in_cheek, 5).
disease_symptom(measles, rashes, 4).

disease_symptom(chickenpox, rashes, 4).
disease_symptom(chickenpox, headache, 3).
disease_symptom(chickenpox, stomachaces, 2).
disease_symptom(chickenpox, fatigue, 2).
disease_symptom(chickenpox, low_grade_fever, 2).

disease_symptom(migraine, light_sensitive, 3).
disease_symptom(migraine, sound_sensitive, 3).
disease_symptom(migraine, headache, 4).
disease_symptom(migraine, nausea, 2).

disease_symptom(strep_throat, fever, 3).
disease_symptom(strep_throat, swolen_tonsils, 4).
disease_symptom(strep_throat, swallowing_pain, 4).
disease_symptom(strep_throat, swollen_lymph_nodes, 3).

disease_symptom(malaria, fever, 3).
disease_symptom(malaria, chills, 3).
disease_symptom(malaria, headache, 3).
disease_symptom(malaria, diarrhea, 2).
disease_symptom(malaria, discomfort, 2).
disease_symptom(malaria, nausea, 2).
disease_symptom(malaria, vomitting, 2).
disease_symptom(malaria, muscle_pain, 2).
disease_symptom(malaria, joint_pain, 2).
disease_symptom(malaria, abdominal_pain, 2).

disease_symptom(pneumonia, cough, 3).
disease_symptom(pneumonia, short_breath, 3).
disease_symptom(pneumonia, high_temperature, 3).
disease_symptom(pneumonia, chest_pain, 3).
disease_symptom(pneumonia, body_ache, 2).
disease_symptom(pneumonia, appetite_loss, 2).
disease_symptom(pneumonia, wheezing, 2).

disease_symptom(covid_19, fever, 3).
disease_symptom(covid_19, cough, 3).
disease_symptom(covid_19, loss_of_taste, 5).
disease_symptom(covid_19, short_breath, 3).
disease_symptom(covid_19, difficulty_breathing, 3).
disease_symptom(covid_19, congestion, 2).
disease_symptom(covid_19, fatigue, 2).
disease_symptom(covid_19, sore_throat, 2).

% --------------------------
% Diagnosis Ranking
% --------------------------

disease_score(Patient, Disease, Score) :-
    setof(Weight, Sym^(
        disease_symptom(Disease, Sym, Weight),
        symptom(Patient, Sym)
    ), Weights),
    sum_list(Weights, Score),
    Score > 0.

print_ranked_diagnosis(Patient) :-
    \+ disease_score(Patient, _, _),
    writeln('We could not identify the sickness based on the symptoms you have provided.'), !.

print_ranked_diagnosis(Patient) :-
    setof(Score-Disease, disease_score(Patient, Disease, Score), ScoredList),
    sort(0, @>=, ScoredList, Ranked),
    writeln('Based on your symptoms, likely conditions are:'),
    print_disease_scores(Ranked).

print_disease_scores([]).
print_disease_scores([Score-Disease | Rest]) :-
    Score >= 3,
    format('DEBUG: Raw - Disease: ~w, Score: ~w~n', [Disease, Score]),
    ( atom(Disease) -> atom_string(Disease, DiseaseStr)
    ; format('WARNING: Non-atom disease detected: ~w~n', [Disease]),
      DiseaseStr = 'Unknown disease'
    ),
    format('~w: Score ~d~n', [DiseaseStr, Score]),
    print_disease_scores(Rest).
print_disease_scores([Score-_|Rest]) :-
    Score < 3,
    print_disease_scores(Rest).

% --------------------------
% Interactive Menu
% --------------------------

start :-
    writeln('--- Medical Diagnosis System ---'),
    writeln('1. Enter patient name'),
    writeln('2. Add symptom'),
    writeln('3. Show diagnosis'),
    writeln('4. Clear patient data'),
    writeln('5. Exit'),
    writeln('------------------------------'),
    read_choice.

read_choice :-
    write('Choose an option (1-5): '),
    read(Choice),
    handle_choice(Choice).

handle_choice(1) :-
    write('Enter patient name: '),
    read(Patient),
    retractall(current_patient(_)),
    asserta(current_patient(Patient)),
    format('Current patient set to ~w.~n', [Patient]),
    nl, start.

handle_choice(2) :-
    current_patient(Patient) ->
        (write('Enter symptom (e.g., fever): '),
         read(Symptom),
         assertz(symptom(Patient, Symptom)),
         format('Symptom ~w added for ~w.~n', [Symptom, Patient]),
         nl, start)
    ;
        (writeln('Please set a patient name first.'), nl, start).

handle_choice(3) :-
    current_patient(Patient) ->
        (print_ranked_diagnosis(Patient), nl, start)
    ;
        (writeln('Please set a patient name first.'), nl, start).

handle_choice(4) :-
    current_patient(Patient) ->
        (retractall(symptom(Patient, _)),
         writeln('Symptoms cleared.'),
         nl, start)
    ;
        (writeln('Please set a patient name first.'), nl, start).

handle_choice(5) :-
    writeln('Exiting the diagnosis system. Goodbye!').

handle_choice(_) :-
    writeln('Invalid option. Please try again.'),
    nl, start.
