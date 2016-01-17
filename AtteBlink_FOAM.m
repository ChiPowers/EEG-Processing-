% Attentional blink paradigm for FOAM.
% Created BY LOT, modified by CEP 02/17/12
% Run in 75 Hz refresh rate!!!!!


clear all; close all; clc

DATA_FOLDER = ['data'];

if (~exist(DATA_FOLDER))
    mkdir(DATA_FOLDER);
end

%--------------------------------------------------------------------------------------------- EXPERIMENT INFO
expName = 'FOAM';							% EXPERIMENT NAME
expDash = '_';								% DASH
expDate = date;								% DATE

expMode = str2num(input('Enter the Experiment Type (0: Practice, 1: Experiment)::', 's'));

expSbj= input('Enter Your Student Number Please::', 's');

expFileName = strcat(expName, expDash, expDate, expDash, expSbj);

% cd /Users/laura/Documents/MATLAB/EmotionalBlink/AtteBlinkET

ClockRandSeed

PRACTICE = 0;	EXPERIMENT = 1;

% Number of Blocks
if (expMode == EXPERIMENT)
    Blocks = 1;
elseif (expMode == PRACTICE)
    Blocks = 1;
end

%Blocks = 1;

% Text Size
Text_Size = 84;

%Create Struct for output
Results = ReadStructsFromText('FOAM_EEG_AB.txt');
T1_ACC = 0;
T2_ACC = 0;
BothACC = 0;

%Manipulated Conditions
Lags = [4 8];
Number_lag_presentation = 24; % How many trials per Lag: 
All_Lags = repmat(Lags,1,Number_lag_presentation); %Create 1 row with repetitions of columns [4 8] = 48 trials
Positions = 4:8; %Potential position of T1
Num_pics_after_T1 = 11; %Max number of pics after T1;
Number_of_trials = length(All_Lags);

% WaitFrames       % Durations are dependent on refresh rates!
Stimulus_WF = 2; % 
Fix_Point_WF = 10; % 
ISI_WF = 2;  % 

% Targets
Distractors = ['A'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'J'; 'K'; 'L'; 'M';...
    'N'; 'P'; 'R'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z'];
Targets = ['2';'3';'4';'5';'6';'7';'8';'9'];
All_distract = repmat(Distractors,1,8);
Total_Num_distract = length(All_distract);

% Colors
TextColor = 255;
BackGround = 127.5;
FixColor = [255 0 0];
LetterColor = 0;

% Event codes
Start_exp = 222;
Start_Trial = 101;
End_trial = 201;
End_exp = 999;

% Create beep
beep = MakeBeep(500,0.067);

% Keyboard
KbName('UnifyKeyNames');
spaceKey = KbName('space');
TargetKeys = ['0'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'];


%% instructions

GeneralInstructions = {'   ';...
    'Two numbers embedded in a series of letters';'will be presented to you.'; '     ';...
    '   ';...
    'The two numbers are always different.';'     ';...
    'After each series of letters, you will decide';...
    'which were the two numbers.';      '     '; '     ';...
    'Please click the mouse to begin'};

Start_trial = {'Please click the mouse to start the trial '};

Break = {'This is a Break'};

EndInstructions = {'End'};


%% EXPERIMENTAL SETTINGS

try
    % Open Screen
    whichScreen = max(Screen('Screens')); %when 2 monitors exist, this opens the screen on the bigger of the 2 monitors
    [theWindow,theRect] = Screen(whichScreen,'OpenWindow',BackGround,[],[],2);

    % Get frame rate from the monitor
    monitorFlipInterval=Screen('GetFlipInterval', theWindow);
    Wait_timeFix = Fix_Point_WF*monitorFlipInterval;
    Wait_timeStm = Stimulus_WF*monitorFlipInterval;
    Wait_timeISI = ISI_WF*monitorFlipInterval;

    HideCursor;

    priorityLevel=MaxPriority(theWindow);


    % COORDINATES
    % Center coordinates
    center_x = theRect(RectRight)/2;
    center_y = theRect(RectBottom)/2;


    % SHOW GENERAL INSTRUCTIONS
    % General Instructions display
    PresentText( theWindow , theRect, GeneralInstructions, TextColor);
    GetClicks;

    for current_block = 1:Blocks

        Randomized_conditions = randperm(Number_of_trials);

        for current_trial = 1:Number_of_trials
            % Choose Lag
            current_lag = All_Lags(Randomized_conditions(current_trial));

            % Choose position of T1
            Possible_pos = randperm(length(Positions));
            current_position = Positions(Possible_pos(1));

            % Distractor order
            Distractor_order = randperm(Total_Num_distract);

            % CHOOSE TARGETS
            % PIC T1
            Pic_Rd_numberT1 = randperm(length(Targets)); % Total Permutes number of targets
            Num_T1 = Pic_Rd_numberT1(1); % Chooses the first target from the permutation

            % PIC T2
            Pic_Rd_numberT2 = randperm(length(Targets)); % Total Permutes number of targets
            Num_T2 = Pic_Rd_numberT2(1); % Chooses the first target from the permutation

            % In case T1 && T2 are the same,
            % choose again T2
            while Num_T1 == Num_T2
                % PIC T2
                Pic_Rd_numberT2 = randperm(length(Targets)); % Total Permuts number of targets
                Num_T2 = Pic_Rd_numberT2(1); % Chooses the first target from the permutation
            end

            % Text Vs
            Screen('TextSize',theWindow, Text_Size);
            Screen('TextStyle',theWindow, 0);
            Screen('TextFont',theWindow,  'Bookman');

            %% PRESENTATION OF THE STIMULI
            PresentText( theWindow , theRect, Start_trial, TextColor);
            GetClicks;

            % Define Font Variables
            Screen('TextSize',theWindow, 28);
            Screen('TextFont',theWindow,  'Bookman');

            Priority(priorityLevel);

            % FOR MORE PRECISE TIMING STIMULI ARE FIRST LOADED, THE FLIPPING
            % IS DELAYED FOR A WAIT TIME (THE DURATION OF THE PREVIOUS STIMULUS)
            % WAIT TIMES DEPEND ON TH REFRESH RATE OF THE MONITOR

            Screen('DrawLine',theWindow, FixColor, center_x - 15, center_y , center_x + 15,  center_y, 4);
            Screen('DrawLine',theWindow, FixColor, center_x , center_y - 15 , center_x, center_y + 15, 4);
            Fix_Start = Screen('Flip', theWindow);
            Snd('Play',beep);

            % TRAIN OF PICTURES
            for current_pres = 1:current_position + Num_pics_after_T1
                % Choose number/letter to present
                if current_pres == current_position
                    curr_pres = Targets(Num_T1);
                elseif current_pres == current_position + current_lag
                    curr_pres = Targets(Num_T2);
                else
                    curr_pres = num2str(All_distract(Distractor_order(current_pres)));
                end

                % Choose wait time for Stimulus
                if current_pres == 1
                    Previous_Start = Fix_Start;
                    Curr_Wait_time = Wait_timeFix;
                else
                    Previous_Start = ISI_Start;
                    Curr_Wait_time = Wait_timeISI;
                end

                % Present target/distractor
                height_curr = RectHeight(Screen('TextBounds', theWindow, curr_pres))/2;
                width_curr = RectWidth(Screen('TextBounds', theWindow, curr_pres))/2;
                Screen('Drawtext',theWindow, curr_pres, center_x-width_curr, center_y-height_curr, LetterColor);
                Stimulus_Start = Screen('Flip', theWindow, Previous_Start+Curr_Wait_time);


                % Present ISI
                Screen('Fillrect',theWindow, BackGround);
                ISI_Start = Screen('Flip', theWindow, Stimulus_Start+Wait_timeStm);
                Priority(0);
                
                if strcmp(curr_pres, Targets(Num_T1)) == 1
                    Results(current_trial).T1 = Targets(Num_T1);
                    Results(current_trial).T1_time = Stimulus_Start - (Fix_Start+Wait_timeFix);
                    Results(current_trial).T1_position = current_position;
                elseif strcmp(curr_pres, Targets(Num_T2)) == 1
                    Results(current_trial).T2 = Targets(Num_T2);
                    Results(current_trial).T2_time = Stimulus_Start - (Fix_Start+Wait_timeFix);
                    Results(current_trial).T2_position = current_position + current_lag;
                end

            end
           %Post-sequence 1000ms fixation before report of T1 and T2
            Screen('DrawLine',theWindow, FixColor, center_x - 15, center_y , center_x + 15,  center_y, 4);
            Screen('DrawLine',theWindow, FixColor, center_x , center_y - 15 , center_x, center_y + 15, 4);
            PostFix_Start = Screen('Flip',theWindow);
            WaitSecs(1);
            
            % Response screen
            responses = 0;
            All_responses = 0;
            Response = {'Which were the two numbers?'};
            PresentText( theWindow , theRect, Response, TextColor);
            while All_responses == 0;
                [ keyIsDown, seconds, keyCode ] = KbCheck;
                if keyIsDown
                    currentKey = KbName(keyCode);
                    if ismember(currentKey,TargetKeys)
                        responses = responses + 1;
                        if responses == 1
                            Key1 = currentKey;
                            Response = {['Which were the two numbers? ' Key1]};
                            PresentText( theWindow , theRect, Response, TextColor);
                            Results(current_trial).Answer1 = Key1;
                        else
                            Key2 = currentKey;
                            All_responses = 1;
                            Response = {['Which were the two number? ' Key1 ',' Key2]};
                            PresentText( theWindow , theRect, Response, TextColor);
                            Results(current_trial).Answer2 = Key2;
                        end

                        WaitSecs(.1);
                    end
                    WaitSecs(.1);
                end
            end

            Results(current_trial).ISI = (Results(current_trial).T2_time - Results(current_trial).T1_time); %calculate time between T1 and T2
            
            WaitSecs(1);
            ITI(theWindow, 0.5, BackGround );
                    %Do Response calculations%
                    if All_responses == 1
            
                        if strcmpi(Results(current_trial).Answer1,Targets(Num_T1)) == 1
                            T1_ACC=T1_ACC+1;
                            Results(current_trial).AnswerT1 = 1;
                        else
                            Results(current_trial).AnswerT1 = 0;
                        end
                        if strcmpi(Results(current_trial).Answer2,Targets(Num_T2)) == 1
                            T2_ACC=T2_ACC+1;
                            Results(current_trial).AnswerT2 = 1;
                        else
                            Results(current_trial).AnswerT2 = 0;
                        end
                        if Results(current_trial).AnswerT1 == 1 && Results(current_trial).AnswerT2 == 1
                           BothACC = BothACC+1;
                        end
                            
                    end

            if current_block ~= Blocks
                PresentText( theWindow , theRect, Break, TextColor);
                waitForKeyPress(spaceKey);
                ITI(theWindow, 0.5, BackGround );
            end
        end
    end
    % END INSTRUCTIONS
    TimeofShow = PresentText( theWindow , theRect, EndInstructions , TextColor);
    % waitForKeyPress(spaceKey);
    
    %Calculate Stats for output
    
    Results(1).Ttl_T1_Acc = T1_ACC / 49;
    Results(1).Ttl_T2_Acc = T2_ACC / 49;
    Results(1).Ttl_BothAcc = BothACC / 49;
    
    
    
    
    
    

    ShowCursor('Arrow')
    GetClicks;
    Screen('CloseAll')

catch %#ok<CTCH>

    psychrethrow(psychlasterror);
    Screen('CloseAll')
end
    %=======================
    % SAVE RESULTS
    %=======================
WriteStructsToText([DATA_FOLDER, '/', expFileName, '.txt'], Results); %write the struct called a to a file%=======================

    % 	figure;
    % 	plot(RESMAT);
    % 	axis([0.5 4.5 0 100]);
    % 	xlabel('Lag');
    % 	ylabel('Accuracy of T2');
    % 	figName= strcat(FIG_FOLDER, '/', expFileName);
    % 	saveas(gcf,figName,'jpg');



