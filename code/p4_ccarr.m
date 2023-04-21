
function [f] = p4_ccarr(x, data)
% sigma(t) = q(t) + alpha2(y(t-1) - q(t-1)) + beta2(sigma(t-1)-q(t-1))
% q(t) = omega + alpha1*y(t-1) + beta1*sigma(t-1)


numData = length(data); % Length of time series


% CCARR coefficients
omega= 0.0001; 
alpha2= 0.1;
beta2= 0.7;
alpha1 = 0.1;
beta1 = 0.9;

% Create an array for conditional variance
sigma = zeros(numData,1);
q = zeros(numData,1);
q(1) = sqrt(2.6709e-04);
sigma(1) = sqrt(2.6709e-02); % Initial volatility (sqrt(variance)) value

% Evaluate conditional volatility 
for i=2:numData
    q(i) = omega + alpha2*(data(i-1)) + beta2*q(i-1);
    sigma(i) =  sqrt(q(i) + alpha1*(data(i-1)-q(i-1))+beta1*(sigma(i-1).^2-q(i-1)));
    
end
% Data that follows CCARR process

omega = x(1);
alpha2 = x(2);
beta2 = x(3);
alpha1 = x(4);
beta1 = x(5);
x(1) = x(1)/1-x(2)-x(3);
numData = size(data(:,1),1);
sigma = zeros(numData,1);

sigma(2) = sqrt( data(2) );

likelihood = 0;
for i=3:numData
    q(i) = omega + alpha2*data(i-1) + beta2*q(i-1);
    sigma(i) =  sqrt(q(i) + alpha1*(data(i-1)-q(i-1))+beta1*(sigma(i-1).^2-q(i-1)));    
    likelihood = likelihood - (log(sigma(i).^2) + data(i)/sigma(i).^2);
end

f = -likelihood;
end