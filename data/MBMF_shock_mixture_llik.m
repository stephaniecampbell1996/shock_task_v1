% To run: >> MBMF_shock_mixture_llik(x,groupdata.subdata)
% When you run wrapper function, this script will be called.
% set_params.m and groupdata.mat should have already been run.

function  LL = MBMF_shock_mixture_llik(x,subdata)

    % parameters
    b = x(1);       % softmax inverse temperature
    lr = x(2);      % learning rate
    lambda = x(3);  % eligibility trace decay
    w = x(4);       % mixing weight

    % initialization
    % Q(s,a): state-action value function for Q-learning
    Qmf = subdata.startvalue*ones(2,2);
    Q2 = subdata.startvalue*ones(2,1);  

    Tm = cell(2,1);
    % transition matrices
    Tm{1} = [1 0; 0 1]; 
    Tm{2} = [1 0; 0 1];        

    % last choice structure
    % M = [0 0; 0 0];   
    % R = [0; 0];      

    N = size(subdata.choice1);

    LL = 0;

    % iterate through all trials
    for t = 1:N

        if subdata.missed(t)==1 % if trial was missed, just skip
            continue
        end

        s1 = subdata.state1(t);
        s2 = subdata.state2(t);
        a = subdata.choice1(t);

        a = a - (s1 == 2)*(2);

        % compute model-based value function
        Qmb = Tm{s1}'*Q2;                                       
        Q = w*Qmb + (1-w)*Qmf(s1,:)';

        LL = LL + b*Q(a) - logsumexp(b*Q);

        % update level 1

        % prediction error. backup with actual choice (i.e., sarsa)
        dtQ(1) = Q2(s2) - Qmf(s1,a);        
        % update TD value function
        Qmf(s1,a) = Qmf(s1,a) + lr*dtQ(1);      

        % update level 2

        % prediction error. (2nd choice)
        dtQ(2) = subdata.shock(t) - Q2(s2);                                        
        % update TD value function
        Q2(s2) = Q2(s2) + lr*dtQ(2);   
        % eligibility trace
        Qmf(s1,a) = Qmf(s1,a) + lambda*lr*dtQ(2);                     

    end

end