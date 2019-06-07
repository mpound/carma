% Matlab script to plot delays
% $Id: delayPlot.m,v 1.3 2004/07/16 20:03:02 rick Exp $
% author: rick hobbs
%
load ./inputDelay.txt
load ./outputDelay.txt
load ./inputDelayActual.txt
tin = inputDelay(:,1);
din = inputDelay(:,2);
tina = inputDelayActual(:,1);
dina = inputDelayActual(:,2);
tout = outputDelay(:,1);
dout = outputDelay(:,2);
maxdiff = 0;
for i = 1:length(tina)
    t1 = tina(i);
    for j = 1:length(tout)
        t2 = tout(j);
        if t1 == t2
            diff(i) = dina(i) - dout(j);
            maxdiff = max(maxdiff, abs(diff(i)));
        end
    end
end
maxdiff
ts = sprintf('Input/Interpolated Delay (max error= %f)',maxdiff);
%subplot(2,1,1)
plot(tin, din, 'bo', tina, dina, 'gx', tout, dout, 'r.');
grid;
title( ts);
xlabel('timestamp');
ylabel('delay');
legend('input to Interpolater', 'Actual delay', 'interpolated delay');
%subplot(2,1,2)
%plot(tina, diff)
%grid;
%ts = sprintf('Delay Error (max error= %f)',maxdiff);
%title( ts);
%xlabel('timestamp');
%ylabel('Error');
