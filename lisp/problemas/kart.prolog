[[domain(capacete),amarelo,azul,branco,verde,vermelho],[domain(nome),bruno,emerson,felipe,fernando,rubens],[domain(idade),'22a','25a','27a','29a','31a'],[domain(idolo),a_prost,f_alonso,k_raikkonen,m_schumacher,n_piquet],[domain(carro),crossover,hatch,pickup,sedan,suv],[domain(equipe),ferrari,mclaren,rbr,renault,williams]].
[[>,ferrari,a_prost],[<,ferrari,williams],[>,rbr,mclaren],[<,rbr,ferrari],[=,sedan,ferrari],[=,mclaren,2],[=,rbr,crossover],[or,[=,pickup,1],[=,pickup,5]],[>,amarelo,suv],[<,amarelo,rbr],[=,pickup,k_raikkonen],[=,[+,f_alonso,1],williams],[=,[-,verde,1],n_piquet],[or,[=,a_prost,1],[=,a_prost,5]],[=,'22a',k_raikkonen],[>,'27a','29a'],[<,'27a','25a'],[>,felipe,'27a'],[=,suv,'29a'],[=,[abs,[-,emerson,'25a']],1],[=,'27a',n_piquet],[>,emerson,a_prost],[<,emerson,sedan],[>,bruno,rubens],[<,bruno,rbr],[=,f_alonso,'25a'],[=,[+,vermelho,1],'25a'],[=,[+,felipe,1],branco]].
