function plotVLMotif(ts_file,motif_f,thres)
if(nargin==2)
    thres=inf;
end
t=load(ts_file);
res=load(motif_f);
tr=res(:,6)./res(:,5);

R=res(tr<=thres,1:4);
Q=res(tr<=thres,6);
L=res(tr<=thres,5);
r=[min(L) max(L)];
split=r(1):(r(2)-r(1))/31:r(2);

P=zeros(1,30);
for i=1:length(split)-2
    Qs=Q;
    Qs(L<split(i) | L>=split(i+1))=10000;
    [~, b]=sort(Qs);
    P(i)=sum(Qs<0.05*split(i)+1);
    B{i}=b;
end

for i=1:length(R)
    for j=1:30
        subplot(5,6,j);
        rank=B{j}(i);
        b=B{j};
        if(i>P(j))
            plot([0 0],[0 0]);
          continue;
        end
        Rs=R(rank,:);
    X=zscore(t(Rs(1,1):Rs(1,1)+L(b(i))));
    plot(X,'Color',[0,0.4470,0.7410],'LineWidth',2)
hold on
X=zscore(t(Rs(1,3):Rs(1,3)+L(b(i))));

disp(['Loc: ' num2str(j)]);

disp(['Length: ' num2str(L(b(i)))]);
disp('Motif Interval A:');
disp(R(b(i),1:2));
disp('Motif Interval B:');
disp(R(b(i),3:4));
%grey = [0.4,0.4,0.4];
plot(X,'Color','r','LineWidth',2)
hold off
    end
   % return;
   disp('Press any buttom to see next page of motifs')
   
   pause;
        disp(i)
end