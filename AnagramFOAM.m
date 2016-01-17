      %CRA script attempt%
clear all; close all; clc

%INITIALIZE FILE AND DIRECTORY SETUP
DATA_FOLDER = ['data'];

if (~exist(DATA_FOLDER))
	mkdir(DATA_FOLDER);
end
%--------------------------------------------------------------------------------------------- EXPERIMENT INFO
expName = 'CRA';							% EXPERIMENT NAME
expDash = '_';								% DASH
expDate = date;								% DATE

expSbj= input('Enter Your Student Number Please::', 's');

expFileName = strcat(expName, expDash, expDate, expDash, expSbj);

%SETUP SCREEN PARAMETERS
%--------------------------------------------------------------------------------------------- GET WINDOW INFO
szScreen = Screen(0, 'Rect');
hzScreen = Screen(0, 'FrameRate');
%----------------------------------------------------------------------------------- COLOR
BLACK = [0 0 0];	WHITE = [255 255 255];		RED = [255 0 0];

%----------------------------------------------------------------------------------- FONTS
txtSize = [28];

%----------------------------------------------------------------------------------- RECTS & SIZE
szCell = 28;	nRow = 3;	nColl = 3;

centerX = (szScreen(3)-szScreen(1))/2;					% GET CENTER
centerY = (szScreen(4)-szScreen(2))/2;
%--------------------------------------------------------------------------------------------- CREATE MAIN WINDOW
MainWindow = Screen(0, 'Openwindow', BLACK);
Screen(MainWindow, 'TextFont', 'Arial');		Screen(MainWindow, 'TextSize', txtSize);

%SETUP DATA PRESENTATION VARIABLES
rand('state', sum(100*clock));

%Solving Strategy Response Target Keys
TargetKeys = ['a';'i'];
ResponseKey = 'space';
Ana_Solutions = 0;
Ins_Solutions = 0;

% Number of Blocks
Blocks = 2;

%LIST OF INSTRUCTIONS


%LOAD CRA PROBLEMS FIRST FOR SPEED AND TIMING ACCURACY
ProbWords = ReadStructsFromText('FOAM Anagrams.txt');
Total_Ins = 0;
Total_Ana = 0;
ANAsum = 0;
INSsum = 0;
%PRESENT INSTRUCTIONS



%PRACTICE PROBLEMS


%EXPERIMENT PROBLEMS - Set 1
try
    for i=randperm(length(ProbWords)) %for the number of words in the list:
        report = 0;
        newword=num2str([]); %clears the newword variable
        StartWord=ProbWords(i).Word; %startword is the answer word
        chars=randperm(length(StartWord)); %counts the number of characters in the word, and randomizes them

        for j=1:length(chars) %sets the counter to the number of characters in the word
            newword(j)=StartWord(chars(j)); %places the character from the initial word in the position of the randomized numbers and put them in a new string.
        end;

        % show fixation cross
        Screen('DrawLine',MainWindow, RED, centerX - 15, centerY , centerX + 15,  centerY, 4);
        Screen('DrawLine',MainWindow, RED, centerX , centerY - 15 , centerX, centerY + 15, 4);
        Fix_Start = Screen('Flip', MainWindow);
        WaitSecs(1);

        % Present problem word
        height_curr = RectHeight(Screen('TextBounds', MainWindow, newword))/2;
        width_curr = RectWidth(Screen('TextBounds', MainWindow, newword))/2;
        Screen('Drawtext',MainWindow, newword, centerX-width_curr, centerY-height_curr, WHITE);
        Present_Start = Screen('Flip',MainWindow);  
        FlushEvents('keyIsDown');
        while GetSecs<Present_Start+7  %Present problems for 15 seconds
            [ keyIsDown, seconds, keyCode ] = KbCheck;  %wait for solution response
            if keyIsDown
                currentKey = KbName(keyCode);
                if ismember(currentKey,ResponseKey)
                    ProbWords(i).SolveTime =GetSecs - Present_Start;
                    ProbWords(i).HasResponse = 1;
                    DrawFormattedText(MainWindow, 'Say the solution out loud.','center','center',WHITE);
                    VerbalSolution = Screen('Flip', MainWindow);
                    WaitSecs(2);
                    FlushEvents('keyIsDown');
                    DrawFormattedText(MainWindow, 'Insight or Analytic?','center','center', WHITE);
                    Strat_Report = Screen('Flip', MainWindow);
                    % Response screen
                    while report < 1;
                        [ keyIsDown, seconds, keyCode ] = KbCheck;
                        if keyIsDown
                            currentKey = KbName(keyCode);
                            if ismember(currentKey,TargetKeys)

                                if currentKey == 'a'
                                    DrawFormattedText(MainWindow,'Analytic','center','center',WHITE);
                                    Strat_Time = Screen('Flip', MainWindow);
                                    Total_Ana = Total_Ana + 1;
                                    ANAsum = ANAsum + ProbWords(i).SolveTime;
                                    ProbWords(i).Strategy = 'Analytic';
                                    report = report + 1;
                                    
                                elseif currentKey == 'i'
                                    DrawFormattedText(MainWindow,'Insight','center','center',WHITE);
                                    Strat_Time = Screen('Flip', MainWindow);
                                    Total_Ins = Total_Ins + 1;
                                    INSsum = INSsum + ProbWords(i).SolveTime;
                                    ProbWords(i).Strategy = 'Insight';
                                    report = report + 1;
                                end
                            
                            end
                            Waitsecs(.2);
                        end

                        Waitsecs(.1);

                    end
                 
                  break
                end
            
            end

        end   
    
    end
%BREAK AFTER 70 PROBLEMS

%COMPUTE OUTPUT AND WRITE TO A TEXT FILE
ProbWords(1).TotalSolved = Total_Ana + Total_Ins;
ProbWords(1).TotalAnalytic = Total_Ana;
ProbWords(1).TotalInsight = Total_Ins;
ProbWord(1).AvgRespTime = (INSsum + ANAsum)/(Total_Ana + Total_Ins);
ProbWords(1).AvgANAresp = ANAsum / Total_Ana;
ProbWords(1).AvgINSresp = INSsum / Total_Ins;

%Save

%Close
ShowCursor('Arrow')
Screen('CloseAll')
catch

    psychrethrow(psychlasterror);
    Screen('CloseAll')
end
    %done
    WriteStructsToText([DATA_FOLDER, '/', expFileName, '.txt'],ProbWords);





