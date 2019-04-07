

% This is a simple expert system for identify the cases of criminal
% where rule are roughly excerpts according to Malaysia Law, Act 574.

% This expert system will try to solve the goal 'criminal case' and ask
% information from the user.

% 'case' is the high level goal that starts the program. Predicate
% 'known/3' is used to remember answers to question, where then
% previous answers will be cleared when starts a new run.

% The rules of identification are the bulk of code which break up the
% problem into identifying criteria of specific cases before identifying
% the actual criminal cases.

% The predicate 'ask' and 'menuask' are used to get information from
% user and remember it.

main :- case.

case:-
  retractall(known(_,_,_)),         % clear stored information
  offences(X),
  write('the suggested offences is '),write(X),nl.  %create output of what kind of offences suggested.
case:-
  write('Further Processing required'),nl.   %create output if the offences is undefined

%rules for criminal laws
identify_theft(property):-
	write('What kind of property suspect has taken?'),
	choice_t1(take_posession),
	write('The suspect take away the posession without consent the accuser.'),nl,
	find_t1(without_inform_consent),
	write('The suspect had severe the property in order to take the posession.'),nl,
	find_t2(severe_the_posession).
identify_theft(property):-
	choice_t1(take_posession),
	find_t1(without_inform_consent).
identify_theft(property1):-
	choice_t1(take_posession),
	write('The suspect did take consent from the owner to take away the posession.'),nl,
	find_t1(with_inform_consent).
identify_theft(animal):-
	choice_t1(take_animal),
	write('The suspect puts bait to lure the specific animal to follow him/her.'),nl,
	find_t1(feed_animal),
	write('The suspect take the animal away without consent from the animals owner.'),nl,
	find_t2(without_inform_consent).
identify_theft(property1):-
	choice_t1(take_animal),
	find_t1(feed_animal),
	write('The suspect take the animal away with acknowledgement from the animals owner.'),nl,
	find_t2(with_inform_consent).
identify_theft(animal):-
	choice_t1(take_animal),
	find_t1(without_inform_consent).
identify_theft(property1):-
	choice_t1(take_animal),
	find_t1(feed_animal),
	write('The suspect take the animal away with acknowledgement from the animals owner.'),nl,
	find_t2(with_inform_consent).
identify_theft(property1):-
	choice_t1(take_animal),
	find_t1(catch_animal),
	write('The suspect take the animal away with acknowledgement from the animals owner.'),nl,
	find_t2(with_inform_consent).
identify_theft(property1):-
	choice_t1(take_animal),
	find_t1(with_inform_consent).
indicate(evidence_of_kill):-
	write('It was confirm that the suspect action had cause the death of the victim. If there is, choose evidence_of_kill, if not, choose no_evidence_of_kill.'),nl,
	evidence(evidence_of_kill),
	write('Does the suspect know his act is deadly?'),nl,
	understand1(known_deadly_act),
	write('Does the suspect intended to do so?'),nl,
	understand2(intentional_act).
indicate(evidence_of_kill_accident):-
	evidence(evidence_of_kill),
	understand1(known_deadly_act),
	write('So the suspect not intended to do so. By accident then?'),nl,
	understand2(by_accident).
indicate(evidence_of_unknown):-
	evidence(evidence_of_kill),
	understand1(known_deadly_act),
	write('The suspect is neither intend or by accident. Someone ask him to do so?'),
	understand2(ask_by_others).
indicate(evidence_of_unknown):-
	evidence(evidence_of_kill),
	understand1(known_deadly_act).
indicate(evidence_of_kill):-
	evidence(evidence_of_kill),
	understand1(not_known_deadly_act),
	write('Is the suspect intended to do such act?'),nl,
	understand2(intentional_act).
indicate(evidence_of_kill_accident):-
	evidence(evidence_of_kill),
	understand1(not_known_deadly_act),
	write('The suspect not-intended to do so. Accident?'),nl,
	understand2(by_accident).
indicate(evidence_of_kill_unknown):-
	evidence(evidence_of_kill),
	understand1(not_known_deadly_act),
	write('Is not accident. Someone ask so?'),nl,
	understand2(ask_by_other).
indicate(evidence_of_kill_unknown):-
	evidence(evidence_of_kill),
	understand1(not_known_deadly_act).
indicate(evidence_of_kill_innocent):-
	evidence(evidence_of_kill),
	understand1(not_known_deadly_act),
	understand2(ask_by_others),
	write('Does the suspect had no idea what had happened?'),nl,
	understand3(no_idea_what_happened).
indicate(evidence_of_unknown):-
	evidence(evidence_of_kill),
	understand1(not_known_deadly_act),
	write('Does the suspect had no idea what had happened?'),nl,
	understand2(no_idea_what_happened).
indicate(evidence_of_unknown):-
	evidence(evidence_of_kill),
	understand1(not_known_deadly_act).
indicate(evidence_of_unknown):-
	evidence(evidence_of_kill),
	understand1(not_known_deadly_act),
	understand2(ask_by_others).
indicate(no_evidence_of_kill):-
	evidence(no_evidence_of_kill),
	write('The suspect just pass by at the scene'),nl,
	seem(pass_by).
rape_will(willing):-
	write('Does the accuser willing to have sexual intercourse with suspect?'),nl,
	rape_identify1(willing),
	write('Does the accuser uses any violent force towards the accuser?'),nl,
	rape_identify2(brutal_force).
rape_will(willing):-
	rape_identify1(willing),
	write('No brutal force used, suspect misconcept the accuser to have sexual intercourse with him?'),nl,
	rape_identify2(deception).
rape_will(willing):-
	rape_identify1(willing).
rape_will(not_willing):-
	write('So the accuser does not willing to have sexual intercourse with the suspect?'),nl,
	rape_identify1(not_willing),
	write('Does the accuser uses any violent force towards the accuser?'),nl,
	rape_identify2(brutal_force).
rape_will(not_willing):-
	write('So the accuser does not willing to have sexual intercourse with the suspect?'),nl,
	rape_identify1(not_willing),
	write('No brutal force used, suspect misconcept the accuser to have sexual intercourse with him?'),nl,
	rape_identify2(deception).
rape_will(not_willing):-
	rape_identify1(not_willing).
divortion(divorce):-
	write('Is the accuser lawfully divorced with the suspect?'),nl,
	divorce(judical_separation),
	rape_will(willing).
divortion(divorce):-
	divorce(judical_separation),
	rape_will(not_willing).
divortion(divorce):-
	write('Havent lawfully divorce but ready to be divorce then? Then should have obtain decree nisi. Do the accuser obtain decree nisi?'),nl,
	divorce(obtain_decree_nisi),
	rape_will(willing).
divortion(divorce):-
	divorce(obtain_decree_nisi),
	rape_will(not_willing).
live(separate):-
	write('Accuser still lawfully married to the suspect. They live separately or together?'),nl,
	living(live_separate),
	write('Is the accuser obtain any authorative warning or order where the suspect cannot approach the accuser in any way?'),nl,
	write('Select injunction_restrain_from_husband if there is, if no authorative warning or order, select without_injunction.'),nl,
	obtain(injunction_restrain_from_husband),
	rape_will(willing).
live(separate):-
	living(live_separate),
	obtain(injunction_restrain_from_husband),
	rape_will(not_willing).
live(separate_legal):-
	living(live_separate),
	obtain(without_injunction),
	rape_will(willing).
live(separate_legal):-
	living(live_separate),
	obtain(without_injunction),
	rape_will(not_willing).
live(together):-
	living(live_together),
	write('Is the accuser obtain any authorative warning or order where the suspect cannot approach the accuser in any way?'),nl,
	write('Select injunction_restrain_from_husband if there is, if no authorative warning or order, select without_injunction.'),nl,
	obtain(injunction_restrain_from_husband),
	rape_will(willing).
live(together):-
	living(live_together),
	obtain(injunction_restrain_from_husband),
	rape_will(not_willing).
live(together_legal):-
	living(live_together),
	obtain(without_injunction),
	rape_will(willing).
live(together_legal):-
	living(live_together),
	obtain(without_injunction),
	rape_will(not_willing).
marriage(not_married):-
	marry(not_married),
	rape_will(willing).
marriage(not_married):-
	marry(not_married),
	rape_will(not_willing).
marriage(once_married):-
	marry(once_married),
	divortion(divorce).
marriage(married):-
	marry(in_marriage),
	live(separate).
marriage(married_legal):-
	marry(in_marriage),
	live(separate_legal).
marriage(married):-
	marry(in_marriage),
	live(together).
marriage(married_legal):-
	marry(in_marriage),
	live(together_legal).
identify_rape(age_below_16):-
	age_rape(age_below_16),
	rape_will(willing).
identify_rape(age_below_16):-
	age_rape(age_below_16),
	rape_will(not_willing).
identify_rape(age_above_16):-
	age_rape(age_above_16),
	write('Select whether the accuser is married with the suspect.'),nl,
	marriage(not_married).
identify_rape(age_above_16):-
	age_rape(age_above_16),
	marriage(once_married).
identify_rape(age_above_16):-
	age_rape(age_above_16),
	marriage(married).
identify_rape(age_above_16_legal):-
	marriage(married_legal),
	age_rape(age_above_16).
identify_kill(recognize_the_dead):-
	write('Does the suspect recognize the dead? choose know_the_dead if recognise, otherwise choose unknown_the_dead'),nl,
	know(know_the_dead),
	indicate(evidence_of_kill).
identify_kill(recognize_the_dead_accident):-
	know(know_the_dead),
	indicate(evidence_of_kill_accident).
identify_kill(recognize_the_dead_innocent):-
	know(know_the_dead),
	indicate(evidence_of_kill_innocent).
identify_kill(recognize_the_dead):-
	know(unknown_the_dead),
	indicate(evidence_of_kill).
identify_kill(recognize_the_dead_accident):-
	know(unknown_the_dead),
	indicate(evidence_of_kill_accident).
identify_kill(recognize_the_dead_innocent):-
	know(unknown_the_dead),
	indicate(evidence_of_kill_innocent).
verify_theft(property):-
	criminal_case(theft),
	identify_theft(property).
verify_theft(property1):-
	criminal_case(theft),
	identify_theft(property1).
verify_theft(animal):-
	criminal_case(theft),
	identify_theft(animal).
verify_kidnap(lawful_guardian):-
	criminal_case(kidnap),
	write('Whats the gender of the person that is been taken away?'),nl,
	gender_criteria(male),
	age_kidnap(below_14),
	write('The victim had no idea where he is been brought to. Select yes if it is true.'),nl,
	identify_mkidnap1(without_acknowledge_the_taken),
	write('The guardian of the victim was unaware where the kid is been brought to. Select yes if it is true.'),nl,
	identify_mkidnap2(without_consent_the_guardian).
verify_kidnap(lawful_guardian):-
	criminal_case(kidnap),
	gender_criteria(male),
	age_kidnap(below_14),
	write('So, the victim actually know where he is been brought to.'),nl,
	identify_mkidnap1(with_acknowledge_the_taken),
	write('The guardian of the victim was unaware where the kid is been brought to. Select yes if it is true.'),nl,
	identify_mkidnap2(without_consent_the_guardian).
verify_kidnap(lawful_guardian):-
	criminal_case(kidnap),
	gender_criteria(female),
	age_kidnap(below_16),
	write('The victim had no idea where she is been brought to. Select yes if it is true.'),nl,
	identify_fkidnap1(without_consent_the_taken),
	write('The guardian of the victim was unaware where the kid is been brought to. Select yes if it is true.'),nl,
	identify_fkidnap2(without_consent_the_guardian).
verify_kidnap(lawful_guardian):-
	criminal_case(kidnap),
	gender_criteria(female),
	age_kidnap(below_16),
	write('So, the victim actually know where she is been brought to.'),nl,
	identify_fkidnap1(with_consent_the_taken),
	write('The guardian of the victim was unaware where the kid is been brought to. Select yes if it is true.'),nl,
	identify_fkidnap2(without_consent_the_guardian).
verify_kidnap(lawful_guardian):-
	criminal_case(kidnap),
	gender_criteria(female),
	age_kidnap(below_14),
	write('The victim had no idea where she is been brought to. Select yes if it is true.'),nl,
	identify_fkidnap1(without_consent_the_taken),
	write('The guardian of the victim was unaware where the kid is been brought to. Select yes if it is true.'),nl,
	identify_fkidnap2(without_consent_the_guardian).
verify_kidnap(lawful_guardian):-
	criminal_case(kidnap),
	gender_criteria(female),
	age_kidnap(below_14),
	write('So, the victim actually know where she is been brought to.'),nl,
	identify_fkidnap1(with_consent_the_taken),
	write('The guardian of the victim was unaware where the kid is been brought to. Select yes if it is true.'),nl,
	identify_fkidnap2(without_consent_the_guardian).
verify_kidnap(away_Malaysia):-
	criminal_case(kidnap),
	write('Where the person is been taken? Away Malaysia or within Malaysia?'),nl,
	identify_away(taken_away_from_Malaysia),
	write('The victim had no idea where he/she is taken to. Select yes if it is true.'),nl,
	identify_kid1(without_consent_the_taken),
	write('The guardian of the victim was unaware where the person is been brought to. Select yes if it is true.'),nl,
	identify_kid2(without_consent_the_guardian).
verify_kidnap(away_Malaysia):-
	criminal_case(kidnap),
	identify_away(taken_away_from_Malaysia),
	write('So, the victim actually know where he/she is been taken to.'),nl,
	identify_kid1(with_consent_the_taken),
	write('The guardian of the victim was unaware where the person is been brought to. Select yes if it is true.'),nl,
	identify_kid2(without_consent_the_guardian).
verify_kidnap(within_Malaysia):-
	criminal_case(kidnap),
	identify_away(taken_within_Malaysia).
verify_rape(age_below_16):-
	criminal_case(rape),
	write('Select the age of the accuser.'),nl,
	identify_rape(age_below_16).
verify_rape(age_above_16):-
	criminal_case(rape),
	identify_rape(age_above_16).
verify_rape(age_above_16_legal):-
	criminal_case(rape),
	identify_rape(age_above_16_legal).
verify_kill(suspect_at_scene):-
	criminal_case(kill),
	identify_kill(recognize_the_dead).
verify_kill(suspect_at_scene_accident):-
	criminal_case(kill),
	identify_kill(recognize_the_dead_accident).
verify_kill(at_the_scene):-
	criminal_case(kill),
	indicate(no_evidence_of_kill).
verify_kill(no_idea_what_happened):-
	criminal_case(kill),
	identify_kill(recognize_the_dead_innocent).
offences(theft):-
	verify_theft(property),
	write('According to Act 574, if a thing is removed from someone posession without asking for consent, it is considered as theft. Hence, ').
offences(theft):-
	verify_theft(animal),
	write('According to Act 574, if an animal is moved without asking consent from the owner, it is considered as theft. Hence, ').
offences(not_theft):-
	verify_theft(property1).
offences(kidnapping_from_Malaysia):-
	verify_kidnap(away_Malaysia),
	write('According to Act 574, whoever persons being taken beyond the limits of Malaysia with acknowledge the person, or other person that is legally authorized to consent behalf of the person, it is said that ').
offences(kidnapping_from_lawful_guardian):-
	verify_kidnap(within_Malaysia),
	verify_kidnap(lawful_guardian),
	write('According to Act 574, whoever male that aged below 14, female that aged below 16, or keep away anyone acknowledgement or keep away the acknowledgement of the persons guardian by taking away the person, it is said that ').
offences(rape):-
	verify_rape(age_below_16),
	write('According to Act 574, if girls age is below than 16, no matter she willing or not, it is considered as rape. Therefore ').
offences(rape):-
	verify_rape(age_above_16),
	write('According to Act 574, if the girl is not married, willing to have sexual intercourse because of threat or deception, it is considered as rape. Therefore ').
offences(not_rape):-
	verify_rape(age_above_16_legal),
	write('According to Act 574, if the girl is married, as long as there is no authorative order or warning where husband cannot approach her, it is not-considered as rape under Malaysia law. Therefore ').
offences(murder):-
	verify_kill(suspect_at_scene),
	write('According to Act 574, as the person know his action will cause death on others, intend to cause death on others, it is considered as murder. Hence, ').
offences(culpable_homicide):-
	verify_kill(suspect_at_scene_accident),
	write('According to Act 574, as the person know his action will cause death on others, carelessly cause death on others, it is considered as culpable_homicide. Hence, ').
offences(innocent):-
	verify_kill(no_idea_what_happened),
	write('As the person does not fulfil the criteria listed according to Act 574, the person is innocent. Hence, ').
offences(not_relevant):-
	verify_kill(at_the_scene),
	write('As the person is just pass by the scene and no evidence that he had cause death on the victim, it is not relevant for the murder case. ').

%create function for system to ask question
find_t1(X):- ask(find_t1,X).
find_t2(X):- ask(find_t2,X).
choice_t1(X):-menuask(choice_t1,X,[take_posession,take_animal]).
understand1(X):- ask(understand1,X).
understand2(X):- ask(understand2,X).
understand3(X):- ask(understand3,X).
seem(X):- ask(seem,X).
evidence(X):- menuask(evidence,X,[evidence_of_kill,no_evidence_of_kill]).
know(X):- menuask(know,X,[know_the_dead,unknown_the_dead]).
rape_identify1(X):- ask(rape_identify1,X).
rape_identify2(X):- ask(rape_identify2,X).
divorce(X):- ask(divorce,X).
obtain(X):- menuask(obtain,X,[injunction_restrain_from_husband,without_injunction]).
age_rape(X):- menuask(age_rape,X,[age_below_16, age_above_16]).
marry(X):- menuask(marry,X,[not_married, once_married, in_marriage]).
living(X):- menuask(living,X,[live_together, live_separate]).
identify_kid1(X):- ask(identify_kid1,X).
identify_kid2(X):- ask(identify_kid2,X).
identify_away(X):-menuask(identify_away,X,[taken_away_from_Malaysia,taken_within_Malaysia]).
identify_mkidnap1(X):- ask(identify_mkidnap1,X).
identify_mkidnap2(X):- ask(identify_mkidnap2,X).
identify_fkidnap1(X):- ask(identify_fkidnap1,X).
identify_fkidnap2(X):- ask(identify_fkidnap2,X).
gender_criteria(X):- menuask(gender_criteria,X,[male,female]).
age_kidnap(X):- menuask(age_kidnap,X,[below_14, below_16]).
criminal_case(X):-mainmenuask(criminal_case,X,[kill,kidnap,rape,theft]).


% 'ask' is help to get the information from the user, and
% remember the response of the users. It will ask the user if it doesn't
% know the answer to a question. Then, it will asserts the answer once
% user provide the response.Two cases of knowledge is recognized, which
% are: 1) the attribute-value is known to be true, 2) the
% attribute-value is known to be false.

% This means an attribute might have multiple values. A third test to
% see if the attribute has another value could be used to enforce
% single valued attributes. (This test is commented out below)

% For this system the menuask is used for attributes which are single
% valued

% 'ask' only deals with simple yes or no answers. a 'yes' is the only
% yes value. any other response is considered a 'no'.

ask(Attribute,Value):-
  known(yes,Attribute,Value),       % succeed if we know its true
  !.                                % and dont look any further
ask(Attribute,Value):-
  known(_,Attribute,Value),         % fail if we know its false
  !, fail.

ask(Attribute,_):-
  known(yes,Attribute,_),           % fail if we know its some other value.
  !, fail.                          % the cut in clause #1 ensures that if
                                    % we get here the value is wrong.
ask(A,V):-
  write(A:V),                       % if we get here, we need to ask.
  write('? (yes or no): '),
  read(Y),                          % get the answer
  asserta(known(Y,A,V)),            % remember it so we dont ask again.
  Y = yes.                          % succeed or fail based on answer.

% 'menuask' is like ask, only it gives the user a menu to to choose
% from rather than a yes on no answer. In this case there is no
% need to check for a negative since 'menuask' ensures there will
% be some positive answer.

menuask(Attribute,Value,_):-
  known(yes,Attribute,Value),       % succeed if we know
  !.
menuask(Attribute,_,_):-
  known(yes,Attribute,_),           % fail if its some other value
  !, fail.

menuask(Attribute,AskValue,Menu):-
  nl,write('The details within '),write(Attribute),write('?'),nl,
  display_menu(Menu),
  write('Please select: '),
  read(Num),nl,
  pick_menu(Num,AnswerValue,Menu),
  asserta(known(yes,Attribute,AnswerValue)),
  AskValue = AnswerValue.           % succeed or fail based on answer

mainmenuask(Attribute,Value,_):-
  known(yes,Attribute,Value),       % succeed if we know
  !.
mainmenuask(Attribute,_,_):-
  known(yes,Attribute,_),           % fail if its some other value
  !, fail.

mainmenuask(Attribute,AskValue,Menu):-
  nl, write('****************************************************************'),nl,
  write('*****************     Main Case to be solved    ****************'),nl,
  write('================================================================'),nl,nl,
  display_menu(Menu),nl,
  write('================================================================'),nl,
  write('Please select the case you want to solve: '),
  read(Num),nl,
  pick_menu(Num,AnswerValue,Menu),
  asserta(known(yes,Attribute,AnswerValue)),
  AskValue = AnswerValue.           % succeed or fail based on answer

display_menu(Menu):-
  disp_menu(1,Menu), !.             % make sure we fail on backtracking

disp_menu(_,[]).
disp_menu(N,[Item | Rest]):-        % recursively write the head of
  write(N),write(' : '),write(Item),nl, % the list and disp_menu the tail
  NN is N + 1,
  disp_menu(NN,Rest).

pick_menu(N,Val,Menu):-
  integer(N),                       % make sure they gave a number
  pic_menu(1,N,Val,Menu), !.        % start at one
  pick_menu(Val,Val,_).             % if they didn't enter a number, use
                                    % what they entered as the value

pic_menu(_,_,none_of_the_above,[]). % if we've exhausted the list
pic_menu(N,N, Item, [Item|_]).      % the counter matches the number
pic_menu(Ctr,N, Val, [_|Rest]):-
  NextCtr is Ctr + 1,               % try the next one
  pic_menu(NextCtr, N, Val, Rest).
