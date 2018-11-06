#Leitura de biblioteca
require(AnalyzeFMRI)

#Leitura dos dados
betas = f.read.volume("grupo37C-P.nii")
dim(betas)

# imagem axial do décimo individuo:
#imagem axial (z=20) do decimo individuo na fatia 20
image(betas[,, 20, 10])


### ANALISE DE GRUPOS - TESTAR BETA DE CADA UM:

Y = betas[20, 20, 20,]
X = matrix(0, 92, 2)
# Como 1: 37 são controle e do 38:92 são pacientes:

# Variável dummy para controles
X[1:37, 1] = 1

# Variável dummy para pacientes:
X[38:92, 2] = 1


# Ajustar o GLM
#o -1 na formula serve para tirar o intercepto
modelo = lm(Y~-1+X[, 1] + X[, 2])
summary(modelo)

# Se olhar o $\beta_1$ são as variáveis do grupo controle e $\beta_2$ do grupo de pacientes.

# Analisando o teste de hipóteses e a ativação dos respectivos controles. Para ativação $beta_1 > 0$.
# Olhando o resultado vemos que o p-valor é muito pequeno, portanto, os controles ativam nesse voxel. O mesmo vale para o grupo de pacientes. 

# Para entender se o controle ativa MAIS que o de pacientes:
#  $\beta_1 > \beta_2$. No R esse teste não é tão simples de se verificar.



### METODO GRUPO DE REFERENCIA
#EXEMPLO caso sera o controle


#Observacao: os 37 primeiros indiv
#sao controles saudáveis e os demais
#sao pacientes com Parkinson
#GLM

Y = betas[20, 20, 20,]
X = matrix(0, 92, 2)
#Intercepto
X[, 1] = 1
#Variavel dummy para Pacientes
X[38:92, 2] = 1

#Ajustar o GLM
modelo = lm(Y~X[, 2])
summary(modelo)


# Analisando especificamente para esse voxel, como $\beta<0$ é como se o grupo de pacientes possui uma ligeira ativação menor que o grupo de controle, Mas ao verificar o p-valor, pode-se verificar que não é significante.
# Para análise dos mapas, analisamos mais o t-valor.
# Fazendo a varredura para todos os voxeis intracranianos (análise de grupo de referência):

  
#GLM em todos os voxels intracranianos
MAPAT = array(0, c(45, 54, 45, 1))

for(xi in 1:45){
  for(yi in 1:54){
    for(zi in 1:45){ 
      if(betas[xi, yi, zi, 1] != 0){
        Y = betas[xi, yi, zi, ]
        X = matrix(0, 92, 2)
        #Intercepto
        X[, 1] = 1
        #Variavel dummy para Pacientes
        X[38:92, 2] = 1
        
        #Ajustar o GLM
        modelo = lm(Y~X[, 2])
        
        #Estatistica T do paciente vs controle
        MAPAT[xi, yi, zi, 1] = summary(modelo)$coef[2, 3]
      }#fecha if
    }}}#fecha o for do xi,yi,zi

#Salva os mapas em arquivos no formato Analyze (IMG/HDR)
f.write.analyze(MAPAT, "MapaControlevsPaciente",
                pixdim = c(4, 4, 4),
                originator = c(23.5, 32.5, 19, 1, 1))



