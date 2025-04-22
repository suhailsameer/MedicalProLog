% Symptoms to be added/removed during runtime
:- dynamic symptom/2.

%--------------------------
% Diagnosis Rules
%--------------------------

%Headache
diagnosis(Patient, headaches) :-
    symptom(Patient, headache).

%Cold
diagnosis(Patient, cold) :-
    symptom(Patient, cough),
    symptom(Patient, sore_throat),
    symptom(Patient, fever).

%Flu
diagnosis(Patient, flu) :-
    symptom(Patient, fever),
    symptom(Patient, phlegm),
    symptom(Patient, body_pains).

%Measles
diagnosis(Patient, measles) :-
    symptom(Patient, fever),
    symptom(Patient, running_nose),
    symptom(Patient, watery_eyes),
    symptom(Patient, small_white_spots_in_cheek),
    symptom(Patient, rashes).

%Chickenpox
diagnosis(Patient, chickenpox) :-
    symptom(Patient, rashes),
    symptom(Patient, headache),
    symptom(Patient, stomachaces),
    symptom(Patient, fatigue),
    symptom(Patient, low_grade_fever).

%Migraine
diagnosis(Patient, migraine) :-
    symptom(Patient, light_sensitive),
    symptom(Patient, sound_sensitive),
    symptom(Patient, headache),
    symptom(Patient, nausea).

%Strep Throat
diagnosis(Patient, strep_throat) :-
    symptom(Patient, fever),
    symptom(Patient, swolen_tonsils),
    symptom(Patient, swallowing_pain),
    symptom(Patient, swollen_lymph_nodes).

%Malaria
diagnosis(Patient, malaria) :-
    symptom(Patient, fever),
    symptom(Patient, chills),
    symptom(Patient, headache),
    symptom(Patient, diarrhea),
    symptom(Patient, discomfort),
    symptom(Patient, nausea),
    symptom(Patient, vomitting),
    symptom(Patient, muscle_pain),
    symptom(Patient, joint_pain),
    symptom(Patient, abdominal_pain).

%Pneumonia
diagnosis(Patient, pneumonia) :-
    symptom(Patient, cough),
    symptom(Patient, short_breath),
    symptom(Patient, high_temperature),
    symptom(Patient, chest_pain),
    symptom(Patient, body_ache),
    symptom(Patient, appetite_loss),
    symptom(Patient, wheezing).

%COVID-19
diagnosis(Patient, covid_19) :-
    symptom(Patient, fever),
    symptom(Patient, cough),
    symptom(Patient,loss_of_taste),
    symptom(Patient, short_breath),
    symptom(Patient, difficulty_breathing),
    symptom(Patient, congestion),
    symptom(Patient, fatigue),
    symptom(Patient, sore_throat).

%Output
print_diagnosis(Patient) :-
    findall(Disease, diagnosis(Patient, Disease), Diseases),
    Diseases \= [],
    format('Based on your symptoms, you may have: ~w~n', [Diseases]).
    %To prevent backtracking and showing multiple diseases at once

print_diagnosis(_) :-
    writeln('We could not identify the sickeness based on the symptoms you have provided.').

% Interactive Menu
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

% Global variable to store patient name
:- dynamic current_patient/1.

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
        (print_diagnosis(Patient), nl, start)
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

