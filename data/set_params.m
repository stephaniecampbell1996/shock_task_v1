% To run and initialize array x: >> x = set_params
% Used by likelihood.m

function param = set_params

    % create parameter structure
    param(1).name = 'inverse temperature';
    param(1).logpdf = @(x) 0;
    param(1).lb = 0;   % lower bound
    param(1).ub = 20;  % upper bound

    param(2).name = 'learning rate';
    param(2).logpdf = @(x) 0;
    param(2).lb = -1; % only thing that is different here
    param(2).ub = 1;

    param(3).name = 'eligibility trace decay';
    param(3).logpdf = @(x) 0;
    param(3).lb = 0;
    param(3).ub = 1;

    param(4).name = 'mixture weight';
    param(4).logpdf = @(x) 0;
    param(4).lb = 0;
    param(4).ub = 1;

end