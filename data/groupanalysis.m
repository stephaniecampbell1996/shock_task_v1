% To run: >> groupdata = groupanalysis
% To save new group data structure array: >> save groupdata groupdata

function groupdata = groupanalysis

    % importing structured data
    groupdata = make_raw_data; 

    % computing # of subjects
    nrsubs = length(groupdata.subdata); 

    % used if participants with a lot of missing data are present
    s = 0; 

    % iterate over all subjects in the array
    for i = 1:nrsubs
        
        subdata = groupdata.subdata(i);
    
        % if the total # of missed trials < 10%
        if mean(subdata.missed) < 0.10 
           
            s = s+1;
            % disp(s); % for debugging
            
            % create new variables
            groupdata.id{s,1} = subdata.subid; % subject id variable 
            groupdata.age{s,1} = subdata.age; % age 
            groupdata.gender{s,1} = subdata.gender; % gender 
            groupdata.face{s,1} = subdata.face; % face stim seen
            groupdata.color{s,1} = subdata.color; % room color seen
            groupdata.i(s,1) = i; % index for subject
            groupdata.nrtrials(s,1) = length(subdata.choice1); % # of trials       
            % groupdata.nrtimeouttrials(i,1) = sum(subdata.missed > 0); % # of missed trials
            groupdata.totaltimeouts(s,1) = sum(subdata.missed); % # of timeouts
            groupdata.rewardrate(s,1) = subdata.rewardrate;
            groupdata.rewardrate_corrected(s,1) = subdata.rewardrate - subdata.average_shock;
        
        end
        
    end
    
end

