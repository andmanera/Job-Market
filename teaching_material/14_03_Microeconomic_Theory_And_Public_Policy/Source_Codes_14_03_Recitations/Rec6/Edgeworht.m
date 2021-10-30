% Contract curve example:
close all
fontSizeAx = 16

X = 10;
Y = 10;

y_c_curve = @(x_a) 2 * Y .* x_a ./ (X + x_a);



y_a = .01:0.01:9.99;
x_a = y_a';
U_a = x_a*y_a;
U_b = (X - x_a).^(2/3)*(Y - y_a).^(1/3);



%Plotting
fig_name = 'Edgeworth';
fig_width = 1600;               % Window Horizontal Size
fig_heigth = 900;               % Window Vertical Size
fig_posx = 100;                 % Window position (Lower left corner)
fig_posy = 100;                 % Window position (Lower left corner)
hh = figure('Position', [fig_posx fig_posy fig_width fig_heigth]);
[~,c_a] = contour(x_a, y_a, U_a','blue', 'ShowText','on');
c_a.LineWidth = 3;
xlim([0 X])
ylim([0 Y])


a = get(gca,'XTickLabel');
set(gca,'XTickLabel',a,'fontsize',fontSizeAx)
ylabel('$y^A$', ...
    'interpreter','latex', 'fontsize', 2*fontSizeAx)
xlabel('$x^A$',...
    'interpreter','latex', 'fontsize', 2*fontSizeAx)

x_to_label = [5];
y_to_label = [10];
 labelpoints(x_to_label, y_to_label, {'$x_B$'}, ...
     'interpreter','latex', 'fontsize', 2* fontSizeAx, 'position', 'N')
 x_to_label = [0];
y_to_label = [0];
 labelpoints(x_to_label, y_to_label, {'$O_A$'}, ...
     'interpreter','latex', 'fontsize', 2* fontSizeAx, 'position', 'SW')
x_to_label = [10];
y_to_label = [5];
 labelpoints(x_to_label, y_to_label, {'$y_B$'}, ...
     'interpreter','latex', 'fontsize', 2* fontSizeAx, 'position', 'E')
 x_to_label = [10];
y_to_label = [10];
 labelpoints(x_to_label, y_to_label, {'$O_B$'}, ...
     'interpreter','latex', 'fontsize', 2* fontSizeAx, 'position', 'NE')
  
 
fname = fig_name;
set(gcf,'PaperPositionMode','auto');
%hgsave(fname);
print('-depsc',fname,'-painters');

hold on

[~,c_b] = contour(x_a, y_a, U_b', 'red', 'ShowText','on');
c_b.LineWidth = 3;

fname = [fig_name '_2'];
set(gcf,'PaperPositionMode','auto');
%hgsave(fname);
print('-depsc',fname,'-painters');


fplot(y_c_curve, [1e-6 X-1e-6], 'LineWidth', 3)
fname = [fig_name '_3'];
set(gcf,'PaperPositionMode','auto');
%hgsave(fname);
print('-depsc',fname,'-painters');

hh = figure('Position', [fig_posx fig_posy fig_width fig_heigth]);
fplot(y_c_curve, [1e-6 X-1e-6], 'LineWidth', 3)

a = get(gca,'XTickLabel');
set(gca,'XTickLabel',a,'fontsize',fontSizeAx)
ylabel('$y^A$', ...
    'interpreter','latex', 'fontsize', 2*fontSizeAx)
xlabel('$x^A$',...
    'interpreter','latex', 'fontsize', 2*fontSizeAx)

fname = [fig_name '_contract'];
set(gcf,'PaperPositionMode','auto');
%hgsave(fname);
print('-depsc',fname,'-painters');



x_to_label = [5];
y_to_label = [10];
 labelpoints(x_to_label, y_to_label, {'$x_B$'}, ...
     'interpreter','latex', 'fontsize', 2* fontSizeAx, 'position', 'N')
 x_to_label = [0];
y_to_label = [0];
 labelpoints(x_to_label, y_to_label, {'$O_A$'}, ...
     'interpreter','latex', 'fontsize', 2* fontSizeAx, 'position', 'SW')
x_to_label = [10];
y_to_label = [5];
 labelpoints(x_to_label, y_to_label, {'$y_B$'}, ...
     'interpreter','latex', 'fontsize', 2* fontSizeAx, 'position', 'E')
 x_to_label = [10];
y_to_label = [10];
 labelpoints(x_to_label, y_to_label, {'$O_B$'}, ...
     'interpreter','latex', 'fontsize', 2* fontSizeAx, 'position', 'NE')
 
 %% PART 2: PPF
 
 beta = 4;
 gamma = 1;
 alpha = 2/3;
 L = 1;
 
grid_p = 0.01:0.5:100;
 
y_b = L./(grid_p*(beta/gamma) + 1);
y_a = L- y_b;

fig_name = 'PPF';
fig_width = 1600;               % Window Horizontal Size
fig_heigth = 900;               % Window Vertical Size
fig_posx = 100;                 % Window position (Lower left corner)
fig_posy = 100;                 % Window position (Lower left corner)
hh = figure('Position', [fig_posx fig_posy fig_width fig_heigth]);

plot(y_a, y_b)


y_a_p =  0.01:0.01:1;
y_b_p =  0.01:0.01:1;
U = (y_a_p').^alpha * (y_b_p).^(1-alpha);
hold on
contour(y_a_p, y_b_p, U')
%%
fontSizeAx=16;
beta = .3;
alpha = .5;
y_b_p =  [0.001:0.01:1 1];
y_a_p = (1 - y_b_p.^(1/beta)).^(alpha);

y_a_c = [0:0.01:3];
y_b_c = [0:0.01:3];
U = y_a_c'*y_b_c;
% 
% figure()
% hold on
% plot(y_a_p, y_b_p)
% contour(y_a_c, y_b_c, U', [0:0.01:0.59])
fig_name = 'PPF';
fig_width = 1600;               % Window Horizontal Size
fig_heigth = 900;               % Window Vertical Size
fig_posx = 100;                 % Window position (Lower left corner)
fig_posy = 100;                 % Window position (Lower left corner)
hh = figure('Position', [fig_posx fig_posy fig_width fig_heigth]);

hold on
plot(y_a_p, y_b_p, 'LineWidth', 3)
contour(y_a_c, y_b_c, U', [0.589 0.589], 'LineWidth', 3)
xlim([0,1.5])
ylim([0,1.5])
% y_a = .7949 y_b = .741
intercept = 2* .7949;
p = .7949/.741;
x_end = intercept/p;
plot([0:0.01:x_end], intercept - p*[0:0.01:x_end], 'LineWidth', 2, 'LineStyle', '-.')

a = get(gca,'XTickLabel');
set(gca,'XTickLabel',a,'fontsize',fontSizeAx)
ylabel('$y^B$', ...
    'interpreter','latex', 'fontsize', 2*fontSizeAx)
xlabel('$y^A$',...
    'interpreter','latex', 'fontsize', 2*fontSizeAx)


fname = [fig_name];
set(gcf,'PaperPositionMode','auto');
%hgsave(fname);
print('-depsc',fname,'-painters');
%p = .7949/.741


%%
hh = figure('Position', [fig_posx fig_posy fig_width fig_heigth]);

hold on
plot(y_a_p, y_b_p, 'LineWidth', 3)
xlim([0,1.5])
ylim([0,1.5])

a = get(gca,'XTickLabel');
set(gca,'XTickLabel',a,'fontsize',fontSizeAx)
ylabel('$y^B$', ...
    'interpreter','latex', 'fontsize', 2*fontSizeAx)
xlabel('$y^A$',...
    'interpreter','latex', 'fontsize', 2*fontSizeAx)


fname = [fig_name '_only'];
set(gcf,'PaperPositionMode','auto');
%hgsave(fname);
print('-depsc',fname,'-painters');

contour(y_a_c, y_b_c, U', 0.589:0.02:.7, 'LineWidth', 3)


% y_a = .7949 y_b = .741
intercept = 2* .7949;
p = .7949/.741;
x_end = intercept/p;
plot([0:0.01:x_end], intercept - p*[0:0.01:x_end], 'LineWidth', 2, 'LineStyle', '-.')


fname = [fig_name '_2'];
set(gcf,'PaperPositionMode','auto');
%hgsave(fname);
print('-depsc',fname,'-painters');
 