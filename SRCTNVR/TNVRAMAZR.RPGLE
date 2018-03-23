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
001100031017     fQsysprt   o    f  132        Printer Oflind(*InOf)
001200180115
001300180115     D DSLS63        E DS                  EXTNAME(FNLS63DS)
001400180115     d kpjba         E DS
001500180115     C     *ENTRY        PLIST
001600180115     C                   PARM                    KPJBA
001700031017
001800180115     c     kvab          klist
001900180115     c                   kfld                    kfgs              3 0
002000180115     c                   kfld                    blpksc
002100180115     c                   kfld                    blprmn
002200180115     c     kBlp          klist
002300180115     c                   kfld                    klnp              3 0
002400180115     c                   kfld                    kaas              4 0
002500180115     c                   kfld                    kmgs              4 0
002600180115     c     kvat          klist
002700180115     c                   kfld                    vabfgs
002800180115     c                   kfld                    vabccm
002900180115     c                   kfld                    vabcmr
003000180115     c                   kfld                    vabcnt
003100180115     c                   kfld                    vabaas
003200180115     c                   kfld                    vablnp
003300180115     c                   kfld                    vabnrs
003400180115     c                   kfld                    vabnsp
003500180115     c
003600180115     c                   eval      klnp=063
003700180115     c                   eval      kfgs=789
003800180115     c                   eval      kaas=2018
003900180115     c                   eval      kmgs=0114
004000180115     c                   except    testa
004100180115       // Se richiesto file pulisco il lancio precedente per filiale
004200180115       setll (063)   wfKPI01l   ;
004300180115       reade (063)   wfKPI01l   ;
004400180115
004500180115       dow  not %eof(wfKPI01l)   ;
004600180115       delete wfKPI000  ;
004700180115
004800180115       reade (063)   wfKPI01l   ;
004900180115       enddo  ;
005000180115     c
005100180115     c     kblp          Setll     Fnblp43l
005200031021do  1c                   Do        *Hival
005300180115     c     kblp          Reade     Fnblp43l
005400180115    2c                   If        %Eof(Fnblp43l)
005500031017     c                   Leave
005600180115    2c                   EndIf
005700180115     c
005800180115     c
005900180115    2c                   if        blpctm='6 '
006000180115     c                             and  blpdse=0
006100180115     c                   add       1             bolle             9 0
006200180115     c     kvab          chain     edivab3l
006300180115    3c                   if        not %found(edivab3l)
006400180115     c                   except    errno
006500180115     c                   add       1             errate1           7 0
006600180115   x3c                   else
006700180115     c* se partita stampo i segnacolli
006800180115    4c                   if        blpft1='T' and blpdbr=20180114
006900180115    5c                   if        vabcad=blpcad and
007000180115     c                             blpncl=vabncl
007100180115     c                   clear                   wfkpi000
007200180115       kpiepo=063     ;
007300180115       kpiaas=blpaas  ;
007400180115       kpilnp=blplnp  ;
007500180115       kpimes=%int(%subst(%editc(blpmgs:'X'):1:2))  ;
007600180115       kpirmn=blprmn  ;
007700180115       kpirma=blprma  ;
007800180115       kpinrs=blpnrs  ;
007900180115       kpinsp=blpnsp  ;
008000180115       kpincl=blpncl  ;
008100180115       kpimgs=blpmgs  ;
008200180115       kpilna=blplna  ;
008300180115       kpirsd=blprsd  ;
008400180115       kpilod=blplod  ;
008500180115       kpiprd=blpprd  ;
008600180115       kpicad=blpcad  ;
008700180115       kpifin=blpfin  ;
008800180115       kpinzd=blpnzd  ;
008900180115       kpir_trzd=%edITC(vabncd:'X')+'/' +%editc(vabnca:'X') ;
009000180115       KPIDLY_C=vablna  ;
009100180115
009200180115       write wfkpi000 ;
009300180115     c                   except    segnac
009400180115     c                   add       1             partite           7 0
009500180115     c                   else
009600180115     c                   except    errodiv
009700180115     c                   add       1             errate2           7 0
009800180115    5c                   endif
009900180115   x4c                   else
010000180115     c                   clear                   dsls63
010100180115     C                   MOVE      BLPAAS        I63AAS
010200180115     C                   MOVE      BLPLNP        I63LNP
010300180115     C                   MOVE      BLPNRS        I63NRS
010400180115     C                   MOVE      BLPNSP        I63NSP
010500180115     C                   MOVEL     DSLS63        KPJBU
010600180115     c*                  CALL      'FNLS63R'
010700180115     C*                  PARM                    KPJBA
010800180115     c     kvat          setll     edivat1l
010900180115     c     kvat          reade     edivat1l
011000180115     c                   dow       not %eof(edivat1l)
011100180115     c                   eval      vatfgs=063
011200180115     c*                  update    edivat00
011300180115
011400180115     c     kvat          reade     edivat1l
011500180115     c                   enddo
011600180115
011700180115     c                   eval      vabfgs=063
011800180115     c*                  update    edivab00
011900180115     c                   add       1             conta             7 0
012000180115     c
012100180115    4c                   EndIf
012200180115    3c                   EndIf
012300180115    2c                   EndIf
012400180115     c
012500031017
012600180115    1c                   EndDo
012700180115     c                   except    tot
012800031017
012900031017     c                   Eval      *InLr = *On
013000031017
013100031017     oQsysprt   e            Testa          2 02
013200180115     o                                         +  5 'Bolle amazon 14/01/18'
013300031017     o                       *Date         y    110
013400031017     o                                          128 'Pag.'
013500031017     o                       Page          z    132
013600180115     o          e            segnac         1
013700031017     o                                           +1 'Spedizione:'
013800180115     o                       BlpAas              +1
013900180115     o                       BlpLnp              +1
014000180115     o                       BlpNrs        z     +1
014100180115     o                       BlpNsp        z     +1
014200180115     o                       Blplna        z     +1
014300180115     o                       Blprmn        z     +1
014400180115     o                                           +2 'segnac'
014500180115     o                       vablna        z     +1
014600180115     o                       vabncd        z     +1
014700180115     o                                           +1 '/'
014800180115     o                       vabnca        z     +1
014900180115     o
015000180115     o          e            errodiv        1
015100180115     o                                           +1 'Spedizione:'
015200180115     o                       BlpAas              +1
015300180115     o                       BlpLnp              +1
015400180115     o                       BlpNrs        z     +1
015500180115     o                       BlpNsp        z     +1
015600180115     o                       Blprmn        z     +1
015700180115     o                                           +2 'errore'
015800180115     o                       vablna              +1
015900180115     o                       vabncl              +1
016000180115     o                       vabrsd              +1
016100180115     o          e            errno          1
016200180115     o                                           +1 'Spedizione:'
016300180115     o                       BlpAas              +1
016400180115     o                       BlpLnp              +1
016500180115     o                       BlpNrs        z     +1
016600180115     o                       BlpNsp        z     +1
016700180115     o                       Blprmn        z     +1
016800180115     o                       Blprma              +1
016900180115     o                       Blpksc              +1
017000180115     o                                           +2 'non trovata'
017100180115     o          e            tot            1
017200180115     o                                           +1 'Totale elaborate'
017300180115     o                       bolle         z     +1
017400180115     o                                           +1 'Totale cancellate e abilit'
017500180115     o                       conta         z     +1
017600180115     o                                           +1 'erorri lna'                   ù
017700180115     o                       errate2       z     +1
017800180115     o                                           +1 'erorri no trov'               ù
017900180115     o                       errate1       z     +1
018000180115     o                                           +1 'partite'                      ù
018100180115     o                       partite       z     +1
