% To run: >> groupdata = make_raw_data
% When you run groupanalysis function, this script will be called.
% Datafiles should already be imported into MATLAB directory.
% Do not remove %#ok<CODEGEN>.

function groupdata = make_raw_data

    % load data from two-step task and subject info files
    load('data.mat'); 
    load('subinfo.mat'); 

    % iterate for all subjects
    for s = 1:length(subinfo) %#ok<USENS>
    
        % compare MTurkIDs from data.mat with MTurkIDs in subinfo.mat
        trials = data(strcmp(cellstr(subinfo{s,1}), cellstr(data(:,1))), 1:16); %#ok<NODEF>

        % leave out MTurkIDs from data
        trials(:,1) = [];
   
        % disp(subinfo{s,1}); % for debugging 
        % disp(data(:,1)); % for debugging 

        % convert cell array to ordinary array of the underlying data type
        trials = cell2mat(trials);

        % trials(trials(:,2) == 1,:) = []; % exclude practice trials 

        % create new structure for each subject

        % subinfo 
        subdata.subid = subinfo{s,1}; % MTurk IDs as subject ID
        subdata.age = subinfo{s,2}; % age
        subdata.gender = subinfo{s,3}; % gender
        subdata.color = subinfo{s,5}; % room color seen
        subdata.face = subinfo{s,6}; % face stim seen
        subdata.total_time = subinfo{s,4}; % total time taken
        
        % two-step task 
        subdata.choice1 = trials(:,1); % choice on each trial
        subdata.practice = trials(:,4); % if practice trial, 1

        % rescale: less shock = higher reward
        subdata.rews(:,1) = (-trials(:,5)+3)/3;
        subdata.rews(:,2) = (-trials(:,6)+3)/3; 

        % timing variables
        subdata.rt_1 = trials(:,7); % first-stage response time
        subdata.rt_2 = trials(:,8); % second-stage response time
        subdata.time_elapsed = trials(:,13); % cumulative time elapsed 
        subdata.missed = trials(:,14); % if timeout, 1
        
        % states variables
        subdata.state1 = trials(:,9); % state1 shown
        subdata.state2 = trials(:,10); % state2 shown

        % if missed trial, reward (shock value) = "-1"
        for t = 1:length(subdata.choice1)
            if subdata.state2(t)~=-1
                subdata.shock(t,1) = subdata.rews(t,subdata.state2(t));
            else
                subdata.shock(t,1) = -1;
            end
        end

        % create variable to track which key participant pressed
        subdata.stim_left = trials(:,11); % door on left
        subdata.stim_right = trials(:,12); % door on right
        for t = 1:length(subdata.choice1)
            if subdata.choice1(t) == subdata.stim_left(t)
                subdata.button_press(t) = 1; % left button (f) pressed
            end
            if subdata.choice1(t) == subdata.stim_right(t)
                subdata.button_press(t) = 2; % right button (j) pressed
            end
            if subdata.choice1(t) == -1
                subdata.button_press(t) = -1; % no button pressed
            end
        end
    
        subdata.trialindex = trials(:,15); % index trials (0 to 124)
        subdata.N = length(subdata.choice1); % # of trials
        
        % used for parameter estimation
        subdata.startvalue = 0.5; 
    
        % participants see different distributions of reward (shock value)
        % find average shock value distribution for given participant
        subdata.average_shock =  mean(subdata.rews(:));
        
        % find average shock value actually delivered by given participant
        subdata.rewardrate = mean(subdata.shock(subdata.missed == 0));

        % NOTE: the degree to which participant minimized shock is given by
        % the difference, subdata.rewardrate - subdata.average_shock

        % individual subject's data structure to group structure
        groupdata.subdata(s,1) = subdata;
        % disp(groupdata.subdata(s,1)); % for debugging 
    
    end

end
