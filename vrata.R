vrata = c(0,0,1)
izbira = sample(c(1,2,3),1,replace=TRUE,prob=c(1/3,1/3,1/3))

# Test, ali res izbere pravilno v 1/3 primerov
napacna = 0
pravilna = 0
for (i in 1:1000){
  izbira = sample(c(1,2,3),1,replace=TRUE,prob=c(1/3,1/3,1/3))
  if (vrata[izbira]==0){
    napacna = napacna + 1
  }
  else{
    pravilna = pravilna + 1
  }
}
pravilna
napacna

# Ne menjaš
napacna = 0
izbira = 0
pravilna = 0
vrata_krajsi = vrata
for (i in 1:10000){
  vrata = c(0,0,1)
  izbira = sample(c(1,2,3),1,replace=TRUE,prob=c(1/3,1/3,1/3))
  
  # Izločimo vrata, ki jih igralec izbere
  vrata_krajsi = vrata[-izbira]
  
  # Če so ostala vrata 0, izločimo prva
  if (sum(vrata_krajsi)==0){
    vrata_krajsi = vrata_krajsi[-1]
  }
  
  # Če so ostala vrata 1 in 0, izločimo 0
  else{
    vrata_krajsi = vrata_krajsi[-which(vrata_krajsi==0)]
  }
  
  # vrata_krajsi vsebuje samo še tista vrata, ki jih ni izbral
  if (vrata_krajsi==1){
    napacna = napacna + 1
  }
  else{
    pravilna = pravilna + 1
  }
}


# Menjaš
napacna_menjava = 0
izbira = 0
pravilna_menjava = 0
vrata_krajsi = vrata
for (i in 1:10000){
  vrata = c(0,0,1)
  izbira = sample(c(1,2,3),1,replace=TRUE,prob=c(1/3,1/3,1/3))
  vrata_krajsi = vrata[-izbira]
  
  if (sum(vrata_krajsi)==0){
    vrata_krajsi = vrata_krajsi[-1]
  }
  else{
    vrata_krajsi = vrata_krajsi[-which(vrata_krajsi==0)]
  }
  
  # Izberemo oziroma menjamo za to, kar ostane v vrata_krajsi
  if (vrata_krajsi==0){
    napacna_menjava = napacna_menjava + 1
  }
  else{
    pravilna_menjava = pravilna_menjava + 1
  }
}

barplot(c(pravilna,napacna,pravilna_menjava,napacna_menjava),
        col = c("green","red","green","red"),
        names = c("Pravilna","Napačna","Pravilna z menjavo","Napačna z menjavo"))
