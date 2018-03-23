000100031017     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200031017
000300031017      *---------------------------------------------------------------*
000400031017      *                                                               *
000500031017      *---------------------------------------------------------------*
000600031017
000700180115     fFnblp43l  if   e           k Disk
000800180115     fedivab3l  if   e           k Disk
000900180115     fedivat1l  if   e           k Disk
001000180115     fwfkpi01l  uf a e           k Disk
001100180116     ffirab01l  If   e           k disk
001200031017     fQsysprt   o    f  132        Printer Oflind(*InOf)
001300180115
001400180115     D DSLS63        E DS                  EXTNAME(FNLS63DS)
001500180115     d kpjba         E DS
001600180115     C     *ENTRY        PLIST
001700180115     C                   PARM                    KPJBA
001800031017
001900180115     c     kvab          klist
002000180115     c                   kfld                    kfgs              3 0
002100180115     c                   kfld                    blpksc
002200180115     c                   kfld                    blprmn
002300180115     c     kvabr         klist
002400180115     c                   kfld                    kfgs              3 0
002500180115     c                   kfld                    blpksc
002600180115     c                   kfld                    rabrmnr
002700180115     c     kBlp          klist
002800180115     c                   kfld                    klnp              3 0
002900180115     c                   kfld                    kaas              4 0
003000180115     c                   kfld                    kmgs              4 0
003100180115     c     kvat          klist
003200180115     c                   kfld                    vabfgs
003300180115     c                   kfld                    vabccm
003400180115     c                   kfld                    vabcmr
003500180115     c                   kfld                    vabcnt
003600180115     c                   kfld                    vabaas
003700180115     c                   kfld                    vablnp
003800180115     c                   kfld                    vabnrs
003900180115     c                   kfld                    vabnsp
004000180115     c     krab          klist
004100180116     c                   kfld                    blpaas
004200180115     c                   kfld                    blplnp
004300180115     c                   kfld                    blpnrs
004400180115     c                   kfld                    blpnsp
004500180115     c
004600180115     c                   eval      klnp=063
004700180116     c                   eval      kfgs=788
004800180115     c                   eval      kaas=2018
004900180115     c                   eval      kmgs=0114
005000180115     c                   except    testa
005100180115       // Se richiesto file pulisco il lancio precedente per filiale
005200180115       setll (063)   wfKPI01l   ;
005300180115       reade (063)   wfKPI01l   ;
005400180115
005500180115       dow  not %eof(wfKPI01l)   ;
005600180115       delete wfKPI000  ;
005700180115
005800180115       reade (063)   wfKPI01l   ;
005900180115       enddo  ;
006000180115     c
006100180115     c     kblp          Setll     Fnblp43l
006200031021do  1c                   Do        *Hival
006300180115     c     kblp          Reade     Fnblp43l
006400180115    2c                   If        %Eof(Fnblp43l)
006500031017     c                   Leave
006600180115    2c                   EndIf
006700180115     c
006800180115     c
006900180115    2c                   if        blpctm='6 '
007000180115     c                             and  blpdse=0
007100180115     c                   add       1             bolle             9 0
007200180115     c     kvab          chain     edivab3l
007300180115    3c                   if        not %found(edivab3l)
007400180115     c                   except    errno
007500180115     c                   add       1             errate1           7 0
007600180115   x3c                   else
007700180115     c* se partita stampo i segnacolli
007800180115    4c                   if        blpft1='T' and blpdbr=20180114
007900180115    5c                   if        vabcad=blpcad and
008000180115     c                             blpncl=vabncl
008100180115     c                   clear                   wfkpi000
008200180115       kpiepo=063     ;
008300180115       kpiaas=blpaas  ;
008400180115       kpilnp=blplnp  ;
008500180115       kpimes=%int(%subst(%editc(blpmgs:'X'):1:2))  ;
008600180115       kpirmn=blprmn  ;
008700180115       kpirma=blprma  ;
008800180115       kpinrs=blpnrs  ;
008900180115       kpinsp=blpnsp  ;
009000180115       kpincl=blpncl  ;
009100180115       kpimgs=blpmgs  ;
009200180115       kpilna=blplna  ;
009300180115       kpirsd=blprsd  ;
009400180115       kpilod=blplod  ;
009500180115       kpiprd=blpprd  ;
009600180115       kpicad=blpcad  ;
009700180115       kpifin=blpfin  ;
009800180115       kpinzd=blpnzd  ;
009900180115       kpir_trzd=%edITC(vabncd:'X')+'/' +%editc(vabnca:'X') ;
010000180115       KPIDLY_C=vablna  ;
010100180115
010200180115       // write wfkpi000 ;
010300180115     c**                 except    segnac
010400180115     c                   add       1             partite           7 0
010500180115     c                   else
010600180115     c*  scrivo bolla accorpante
010700180115     c                   clear                   wfkpi000
010800180115       kpiepo=063     ;
010900180115       kpiaas=blpaas  ;
011000180115       kpilnp=blplnp  ;
011100180115       kpimes=%int(%subst(%editc(blpmgs:'X'):1:2))  ;
011200180115       kpirmn=blprmn  ;
011300180115       kpirma=blprma  ;
011400180115       kpinrs=blpnrs  ;
011500180115       kpinsp=blpnsp  ;
011600180115       kpincl=blpncl  ;
011700180115       kpimgs=blpmgs  ;
011800180115       kpilna=blplna  ;
011900180115       kpirsd=blprsd  ;
012000180115       kpilod=blplod  ;
012100180115       kpiprd=blpprd  ;
012200180115       kpicad=blpcad  ;
012300180115       kpifin=blpfin  ;
012400180115       kpinzd=blpnzd  ;
012500180115       kpir_trzd=%edITC(vabncd:'X')+'/' +%editc(vabnca:'X') ;
012600180115       KPIDLY_C=vablna  ;
012700180115
012800180115       write wfkpi000 ;
012900180115     c* bolla accorpata cerco segnacollo
013000180116     c     krab          setll     firab01l
013100180116     c     krab          reade     firab01l
013200180116     c                   dow       not %eof(Firab01l)
013300180115     c                   if        blprmn<>rabrmnr
013400180115     c     kvabr         chain     edivab3l
013500180115     c                   if        %found(edivab3l)
013600180115     c                   clear                   wfkpi000
013700180115       kpiepo=063     ;
013800180115       kpiaas=blpaas  ;
013900180115       kpilnp=blplnp  ;
014000180115       kpimes=%int(%subst(%editc(blpmgs:'X'):1:2))  ;
014100180115       kpirmn=blprmn  ;
014200180115       kpirma=blprma  ;
014300180115       kpinrs=blpnrs  ;
014400180115       kpinsp=blpnsp  ;
014500180115       kpincl=blpncl  ;
014600180115       kpimgs=blpmgs  ;
014700180115       kpilna=blplna  ;
014800180115       kpirsd=blprsd  ;
014900180115       kpilod=blplod  ;
015000180115       kpiprd=blpprd  ;
015100180115       kpicad=blpcad  ;
015200180115       kpifin=blpfin  ;
015300180115       kpinzd=blpnzd  ;
015400180115       kpir_trzd=%edITC(vabncd:'X')+'/' +%editc(vabnca:'X') ;
015500180115       KPIDLY_C=vablna  ;
015600180115
015700180115       write wfkpi000 ;
015800180115       endif;
015900180115       endif ;
016000180115     c
016100180116     c     krab          reade     firab01l
016200180115     c                   enddo
016300180115     c
016400180115     c                   add       1             accorp2           7 0
016500180115    5c                   endif
016600180115   x4c                   endif
016700180115    3c                   EndIf
016800180115    2c                   EndIf
016900180115     c
017000031017
017100180115    1c                   EndDo
017200180115     c                   except    tot
017300031017
017400031017     c                   Eval      *InLr = *On
017500031017
017600031017     oQsysprt   e            Testa          2 02
017700180115     o                                         +  5 'Bolle amazon 14/01/18'
017800031017     o                       *Date         y    110
017900031017     o                                          128 'Pag.'
018000031017     o                       Page          z    132
018100180115     o          e            errno          1
018200180115     o                                           +1 'Spedizione:'
018300180115     o                       BlpAas              +1
018400180115     o                       BlpLnp              +1
018500180115     o                       BlpNrs        z     +1
018600180115     o                       BlpNsp        z     +1
018700180115     o                       Blprmn        z     +1
018800180115     o                       Blprma              +1
018900180115     o                       Blpksc              +1
019000180115     o                                           +2 'non trovata'
019100180115     o          e            tot            1
019200180115     o                                           +1 'Totale elaborate'
019300180115     o                       bolle         z     +1
019400180115     o                                           +1 'accorpate'                   ù
019500180115     o                       accorp2       z     +1
019600180115     o                                           +1 'erorri no trov'               ù
019700180115     o                       errate1       z     +1
019800180115     o                                           +1 'partite'                      ù
019900180115     o                       partite       z     +1
