
function [f] = p4_carr(x,data)
% sigma(t) = omega + alpha*y(t-1) + beta*sigma(t-1)



numData = length(data); % Length of time series

% CARR coefficients
omega= 0.0001; 
alpha= 0.3;
beta= 0.6;

% an array for conditional variance
sigma = zeros(numData,1);
sigma(1) = sqrt(data(1)); % Initial volatility (sqrt(variance)) value


% Evaluate conditional volatility 
for i=2:numData
    sigma(i) = sqrt( omega+ alpha*(data(i-1)) + beta*(sigma(i-1)^2));
end
% Data that follows CARR process

omega = x(1);
alpha = x(2);
beta = x(3);
x(1) = x(1)/1-x(2)-x(3);

sigma = zeros(numData,1);
sigma(2) = sqrt( data(2) );
likelihood = 0;
    for i=3:numData
    sigma(i) = sqrt( omega + alpha*(data(i-1))+ beta*(sigma(i-1)^2) );
    likelihood = likelihood + (-log(sigma(i).^2) - data(i)/sigma(i).^2);

f = -likelihood;
    end