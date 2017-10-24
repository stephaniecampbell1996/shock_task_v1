% To run: >> results = wrapper(subjects)
% To run for selected # of participants: >> results = wrapper(1:33)

function results = wrapper(subjects)

    load '../groupdata.mat' 

    % if participant does not provide any input arguments
    % i.e. the parameter estimation will be carried out for all subjects
    if ~exist('subject','var')
        subjects = 1:length(groupdata.id);
    end

    
    % # of times you want to run the likelihood function
    % the more the better, since you won't get stuck in local mimima
    nstarts = 100; 

    data = groupdata.subdata(groupdata.i(subjects));

    % run optimization
    disp(['Fitting subjects ', num2str(subjects)]) % display which subject whose data is being processed
    params = set_params; % import parameters initialized
    f = @(x,data) MBMF_shock_mixture_llik(x, data); % apply the function to all elements of data array
    results = mfit_optimize(f, params, data, nstarts); % execution requires Gershman's mfit toolbox

    % add additional information to the results file
    results.subid = groupdata.id(subjects); 
    results.age = groupdata.age(subjects); 
    results.gender = groupdata.gender(subjects); 
    results.face = groupdata.face(subjects); 
    results.color = groupdata.color(subjects); 
    results.totaltimeouts = groupdata.totaltimeouts(subjects); 
    results.rewardrate_corrected = groupdata.rewardrate_corrected(subjects);

    % save([directory,'/results_bin',nu)],'results')
    % writing the results file 
    % csvwrite('two_step_results.csv', results.x)
    
end