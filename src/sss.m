w = load('wdata.txt');
y = load('ylineardata.txt');
x = load('xdata.txt');
x = x';
[m n] = size(x);
x = (x - repmat(mean(x,2),1,n)) ./ repmat(sqrt(var(x'))',1,n);
x = x';

model = [12;21;50];
X = x(w'>0,model);

nu = 1.0; %tauprior
nu = nu + sum(w.*y.*y);