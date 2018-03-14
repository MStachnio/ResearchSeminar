%% Loading of the Data:

Data = xlsread('Data.xlsx');

%% Rework the data: 

% J'ai pris la valeur "mid"
SP_500 =array2timetable(Data(:,3),'RowTimes',datetime(Data(:,1),'ConvertFrom','excel'));

SP_500_Volume = array2timetable(Data(:,6),'RowTimes',datetime(Data(:,5),'ConvertFrom','excel'));
SP_500_Volume_Call = array2timetable(Data(:,7),'RowTimes',datetime(Data(:,5),'ConvertFrom','excel'));
SP_500_Volume_Put = array2timetable(Data(:,8),'RowTimes',datetime(Data(:,5),'ConvertFrom','excel'));
SP_500_Ratio_Put_Call = array2timetable(Data(:,9),'RowTimes',datetime(Data(:,5),'ConvertFrom','excel'));

Gold = array2timetable(Data(:,13),'RowTimes',datetime(Data(:,11),'ConvertFrom','excel'));

Vix = array2timetable(Data(:,16),'RowTimes',datetime(Data(:,15),'ConvertFrom','excel'));

US_Libor_OIS = array2timetable(Data(:,21),'RowTimes',datetime(Data(:,19),'ConvertFrom','excel'));

CDS_JPM = array2timetable(Data(:,32),'RowTimes',datetime(Data(:,31),'ConvertFrom','excel'));
CDS_CITI = array2timetable(Data(:,36),'RowTimes',datetime(Data(:,35),'ConvertFrom','excel'));
CDS_BOFA = array2timetable(Data(:,40),'RowTimes',datetime(Data(:,39),'ConvertFrom','excel'));

SP_500_Index_Turnover = array2timetable(Data(:,44),'RowTimes',datetime(Data(:,43),'ConvertFrom','excel'));
SP_1_Index  = array2timetable(Data(:,47),'RowTimes',datetime(Data(:,46),'ConvertFrom','excel'));

%% Sychronize (On pourra aussi diviser en 2 période)

New_Data =synchronize(SP_500,SP_500_Volume,SP_500_Volume_Call,SP_500_Volume_Put, ...
                        SP_500_Ratio_Put_Call ,Gold,Vix,US_Libor_OIS, ...
                        CDS_JPM,CDS_CITI,CDS_BOFA,SP_500_Index_Turnover,SP_1_Index, ...
                        'intersection');
                    
New_Data.Properties.VariableNames ={'SP_500','SP_500_Volume','SP_500_Volume_Call',...
    'SP_500_Volume_Put','SP_500_Ratio_Put_Call','Gold','Vix','US_Libor_OIS', ...
   'CDS_JPM','CDS_CITI','CDS_BOFA', 'SP_500_Index_Turnover','SP_1_Index'};   


%% Extraction PCA for the CDS:

Data_PCA =(table2array(New_Data(:,9:11)));
Data_PCA_time =New_Data.Time ;

%the Principal Component Analysis:

[coeff,score,latent,tsquared,explained] = pca(Data_PCA);

explained_Data_PCA = explained;

explained_Data_PCA(1:2,:)


%Extraction of the First Component:
P1_risk  = Data_PCA*coeff(:,1);

P1_risk = array2table(P1_risk);
P1_risk= table2timetable(P1_risk,'RowTimes',Data_PCA_time);

figure;
fig = gcf;
fig.PaperUnits = 'centimeters';
fig.PaperPosition = [1 7 18 8];
plot(P1_risk.Time,P1_risk.Variables,'linewidth',1.2, 'color', 'r')
legend(P1_risk.Properties.VariableNames,'Interpreter','none','FontSize',8','Location','northeast');
title('Principal Component of the 5 year U.S CDS of Financial Sector')
xlabel('Year')
ylabel('Basis Points')
grid on 
print('P1','-dpdf','-r300');


New_Data = [New_Data ,P1_risk] ;


%JPM= timetable2table(CDS_JPM);
%JPM = table2array(JPM(:,2));
%JPM = JPM(5:length(JPM),:);
%plot(JPM)
 %% Export the data: 
 
New_Data = timetable2table(New_Data);
 
filename ='New_Data.xlsx';
writetable(New_Data,filename);

%% END


