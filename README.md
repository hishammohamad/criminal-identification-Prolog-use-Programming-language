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
