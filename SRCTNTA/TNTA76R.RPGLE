000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200061127      * TNTA76R *----------------------------------------------------*
000300061127      *        Elenco clienti con codice fiscale o partita iva diversi
000400940726      *--------------------------------------------------------------*
000500061130     fazorg01l  if   e           k disk
000600061130     fCnAco04l  if   e           k disk
000700061130     fCnaco00f  if   e           k disk    rename(cnaco000:cnacofis)
000800061130     F                                     PREFIX(ctr_)
000900061130     fCnind00F  if   e           k disk
001000061127     fCnind02l  if   e           k disk    RENAME(CNIND000:CNIND002)
001100061130     F                                     PREFIX(ctr_)
001200061127     fcnind03l  if   e           k disk    rename(cnind000:cnind003)
001300061130     F                                     PREFIX(Ctr_)
001400061130     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001500061130     d
001600061130     D TE              S             66    DIM(20) CTDATA PERRCD(1)
001700061130     D $pog            s              3    dim(250)
001800061130     D $DIG            s              1    dim(20)
001900061130     D $ARG            s              3    dim(50)
002000061127     d
002100061127     d kpjba         e ds
002200061127     d tnta75ds      e ds
002300061130     D TIBS02DS      E DS
002400061127     d Tibs34ds      e ds
002500061127     d Azuteds       e ds                  Extname(Azute00f)
002600061130     D TRUL31DS      E DS
002700061130     D TRUL31DS2     E DS
002800061130     d dDatiute      e ds
002900061130     d dLat          e ds
003000061130     d dute01        e ds
003100061127     D Kut             S                   LIKE(indkut) inz(1)
003200061127     D Kcdf            S                   LIKE(indcdf) inz
003300061127     D Kiva            S                   LIKE(indiva)
003400061130     d wabi            s                   like(UteAut)
003500061130     d recok           s              1
003600021212      *--------------------------------------------------------------*     *****
003700061130     c
003800061130     c                   if        duterr<>'E'
003900061130     c* Stampo testata
004000061130     c                   except    testa
004100061127     c
004200061130     c* Leggo in ordine di ragione sociale, per i clienti dei p.o. abilitati
004300061127     c
004400061127     c     kaco          setll     cnaco04l
004500061127     c     kaco          reade     cnaco04l
004600061127     c
004700061127    1c                   dow       not %eof(cnaco04l)
004800061127     c* Verifico se p.o. abilitato
004900061130     c                   movel     acoksc        w003a             3
005000061130     c     w003a         lookup    $pog                                   30
005100061127    2c                   if        *in30
005200061130     c* Verifico la parzializzazione immessa a video
005300061130     c                   movel     acoksc        w0030             3 0
005400061130     c                   if        ita75cdis <>*blanks or ita75care>0
005500061130     c     w0030         chain     azorg01l
005600061130     c                   if        not %found(azorg01l)
005700061130     c                   clear                   orgcar
005800061130     c                   clear                   orgfl3
005900061130     c                   endif
006000061130     c                   endif
006100061130     c
006200061130     c
006300061130     c* Selezioni a video
006400061130     c                   eval      recok='S'
006500061130     c                   select
006600061130   2ac                   when      ita75cpo>0 and w0030<>ita75cpo
006700061130     c                   eval      recok='N'
006800061130     c                   when      ita75care>0 and orgcar<>ita75care
006900061130     c                   eval      recok='N'
007000061130     c                   when      ita75cdis<>' ' and ita75cdis<>orgfl3
007100061130     c                   eval      recok='N'
007200061130     c                   endsl
007300061130     c
007400061130     c* Escludo codici 8888 o 9999
007500061130     c                   move      acoksc        w0040             4 0
007600061130     c                   if        w0040=9999 or w0040=8888
007700061130     c                   eval      recok='N'
007800061130     c                   endif
007900061130     c
008000061130     c                   if        recok='S'
008100061127     c* chaIN SU CNIND00F
008200061127     C     KIND          CHAIN     CNIND00F
008300061127     c* Escludo la partiva iva : PRIVATO  o iso = $$
008400061127     c*                          vuota o = 00000000000
008500061127    3c                   if        indiva<>*blanks and indiva<>'00000000000    '
008600061130     c                             and %subst(indiva:1:2)<>'$$' and
008700061130     c                             indcdf<>*blanks
008800061127     C     'PRIVATO'     SCAN      indiva                                 30
008900061127    4c                   if        not *in30
009000061127     C
009100061127     c* VERIFICO se ci sono altri  codici con la stessa p.iva
009200061127     C*  E CDF DIVERSO
009300061127     c                   eval      kiva=indiva
009400061127     c
009500061127     c     kind02a       setll     cnind02l
009600061127     c     kind02a       reade     cnind02l
009700061127    5c                   dow       not %eof(cnind02l)
009800061130     c* Se non è il codice letto, stampo
009900061130    6c                   if        ctr_indcdf<>*blanks and
010000061130    6c                             ctr_indCDF<>indcdf
010100061127     c* Stampo
010200061127     c                   seton                                        01
010300061127     c                   EXSR      STAMPA
010400061127     c                   setoff                                       01
010500061127    6c                   endif
010600061127     c
010700061127     c     kind02a       reade     cnind02l
010800061127    5c                   enddo
010900061130    4c                   endif
011000061127     c
011100061130     c                   eval      kcdf=indcdf
011200061127     c     kind03a       setll     cnind03l
011300061127     c     kind03a       reade     cnind03l
011400061130    4c                   dow       not %eof(cnind03l)
011500061130     c* Se ha stesso cod fiscale e <> p.iva stampo
011600061130    5c                   if        ctr_indiva<>indiva  and ctr_indiva<>*blanks
011700061127     c* Stampo
011800061127     c                   seton                                        02
011900061127     c                   EXSR      STAMPA
012000061127     c                   setoff                                       02
012100061130    5c                   endif
012200061127     c
012300061127     c     kind03a       reade     cnind03l
012400061130    4c                   enddo
012500061127    3c                   endif
012600061130   2ac                   endif
012700061130    2c                   endif
012800061127     c
012900061127     c     kaco          reade     cnaco04l
013000061127    1c                   enddo
013100061130    c
013200061130    cc                   except    fine
013300061130     c                   endif
013400021212      *
013500030122     C                   SETON                                        LR
013600061127      *------------------------------------------------------------------------*
013700061127      * ROUTINE INIZIALE
013800061127      *------------------------------------------------------------------------*
013900061127     c     *Inzsr        BegSr
014000061130     c     *ENTRY        PLIST
014100061130     c                   PARM                    kpjba
014200061130     c                   movel     kpjbu         tnta75ds
014300061130     c
014400061130     c                   time                    wtime             6 0
014500061127
014600061127     c     *dtaara       define    §azute        azuteds
014700061127     c     *dtaara       define    §datiute      ddatiute
014800061127     c                   in(E)     *dtaara
014900061127     c                   If        %error  or rsut = *blanks
015000061127     c                   Clear                   Tibs34ds
015100061127     c                   Call      'TIBS34R'
015200061127     c                   Parm                    Tibs34ds
015300061127     c                   In        *dtaara
015400061127     c                   EndIf
015500061127
015600061127     c                   Clear                   wabi
015700061127     c                   Clear                   dLat
015800061127
015900061127      * Verifica errori e autorità profilo
016000061127s   1c                   Select
016100061127      * se ho errori nei dati utente esco dal pgm
016200061127w   1c                   When      DutErr = 'E'
016300061127      * se non c'è l'abilitazione
016400061127      * --> se 1° livello, abilitazioni al terminal
016500061127      *     se 2° livello, abilitazioni al punto operativo
016600061130      *     se sede tutta l'azienda
016700061127w   1c                   When      UteAut = *Blanks
016800061127if  2c                   If        DutLpo = '1'
016900061127     c                   Eval      wabi   = 'TP'
017000061127e   2c                   EndIf
017100061127if  2c                   If        DutLpo = '2'
017200061127     c                   Eval      wabi   = 'PO'
017300061127e   2c                   EndIf
017400061127if  2c                   If        DutLpo = 'S'
017500061130     c                   Eval      wabi   = 'AZ'
017600061127e   2c                   EndIf
017700061127      * carica le abilitazioni del profilo
017800061127x   1c                   Other
017900061127     c                   Movel     UteFaf        Dute01
018000061127     c                   Eval      wabi = UteAut
018100061127e   1c                   EndSl
018200061127
018300061127      * controllo se ok l'abilitazione dell'utente
018400061127     c                   Clear                   Tibs02ds
018500061127     c                   Eval      T02Mod = 'C'
018600061127     c                   Eval      T02Sif = knsif
018700061127     c                   Eval      T02Cod = 'LAT'
018800061127     c                   Movel(p)  wabi          T02Ke1
018900061127     c                   Call      'TIBS02R'
019000061127     c                   Parm                    kpjba
019100061127     c                   Parm                    Tibs02ds
019200061127if  1c                   If        T02Err = *Blanks
019300061127     c                   Eval      dLat = T02Uni
019400061127e   1c                   EndIf
019500061127      * errore o non abilitato
019600061127     c                   clear                   TRUL31DS
019700061127     c                   clear                   TRUL31DS2
019800061127if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
019900061127   x1c                   Else
020000061127      * Reperimento dei P.O. gestibili dall'utente
020100061127     c                   eval      I31abi = wabi
020200061130     c                   eval      I31cdi = dutdis
020300061130     c                   eval      I31car = dutare
020400061130     c                   eval      I31cpo = dutpou
020500061127     c                   call      'TRUL31R'
020600061127     c                   parm                    KPJBA
020700061127     c                   parm                    TRUL31DS
020800061127     c                   parm                    TRUL31DS2
020900061127if  2c                   if        O31pog > *zeros
021000061127     c                   movea     O31pog        $pog
021100061127     c                   movea     O31arg        $arg
021200061127     c                   movea     O31dig        $dig
021300061127x   2c                   else
021400061127     c                   clear                   $pog
021500061127     c                   clear                   $arg
021600061127     c                   clear                   $dig
021700061127e   2c                   endif
021800061127    1c                   endif
021900061127     c
022000061127     c     kind03a       klist
022100061127     c                   kfld                    kut
022200061127     c                   kfld                    dutkci
022300061127     c                   kfld                    kcdf
022400061127     c     kind02a       klist
022500061127     c                   kfld                    kut
022600061127     c                   kfld                    dutkci
022700061127     c                   kfld                    kiva
022800061127     c     kaco          klist
022900061127     c                   kfld                    kut
023000061127     c                   kfld                    dutkci
023100061127     c     kIND          klist
023200061127     c                   kfld                    kut
023300061130     c                   kfld                    ACOkcc
023400061127     c                   kfld                    ACOkSC
023500061130     c     kacofis       klist
023600061130     c                   kfld                    kut
023700061130     c                   kfld                    ctr_indkcc
023800061130     c                   kfld                    ctr_indksc
023900061127     c
024000061127     c                   ENDSR
024100061127      *------------------------------------------------------------------------*
024200061127      * STAMPA
024300061127      *------------------------------------------------------------------------*
024400061127     c     STAMPA        BegSr
024500061130     c                   movel     acorag        starag           40
024600061130     c*
024700061130     c     kacofis       chain     cnaco00f
024800061130     c                   if        not %found(cnaco00f)
024900061130     c                   eval      ctr_acorag=*all'?'
025000061130     c                   endif
025100061130     c
025200061130     c                   if        *inof
025300061130     c                   except    testa
025400061130     c                   setoff                                       of
025500061130     c                   endif
025600061130     c
025700061130     c                   except    detta
025800061130     c
025900061127     c                   ENDSR
026000061130     c*----------------------------------------------------------------
026100061130     OPRTF198   E            TESTA            02
026200061130     O                       TE(1)              132
026300061130     O                       TE(2)              198
026400061130     O                       RSUT                20
026500061130     O                       *DATE              180 '  /  /    '
026600061130     O                       PAGE          Z    198
026700061130     O          E            TESTA       1
026800061130     O                       KNSIF               10
026900061130     O                       KNMUS               22
027000061130     O                       wtime              179 '  :  :  '
027100061130     O          E            TESTA            05
027200061130     O                       TE(3)               66
027300061130     O                       TE(4)              132
027400061130     o                       ita75cpo      z     29
027500061130     o                                         +  0 '-'
027600061130     o                       ita75dpo          +  0
027700061130     o                       ita75care     z     80
027800061130     o                                         +  0 '-'
027900061130     o                       ita75dare         +  0
028000061130     o                       ita75cdis          134
028100061130     o                                         +  0 '-'
028200061130     o                       ita75ddis         +  0
028300061130     O          E            TESTA            07
028400061130     O                       TE(5)               66
028500061130     O                       TE(6)              132
028600061130     O                       TE(7)              198
028700061130     O          E            TESTA            08 08
028800061130     O                       TE(8)               66
028900061130     O                       TE(10)             132
029000061130     O                       TE(12)             198
029100061130     O          E            DETTA       1
029200061130     o                       acoksc               7
029300061130     o                                            9 '-'
029400061130     o                       starag           +   1
029500061130     o                       kiva             +   1
029600061130     o                       kcdf             +   1
029700061130     o                                        +   2 '|'
029800061130     o                       ctr_indiva       +   1
029900061130     o                       ctr_indcdf       +   1
030000061130     o                       ctr_indksc       +   2
030100061130     o                                        +   1 '-'
030200061130     o                       ctr_acorag       +   1
030300061130     O          E            DETTA       0  0
030400061130     o               02      ctr_indiva         104
030500061130     o               01      ctr_indcdf       +   1
030600061130     O          E            FINE             65
030700061130     O                       TE(14)             132
030800061130**
030900061130   ** ELENCO CLIENTI CON DIVERSA PARTITA IVA O CODICE FISCALE **     1
031000061130                       TNTA76R                           PAG. XXXX   2
031100061130SELEZIONI   P.Operativo : xxx-xxxxxxxxxxxxxxxxxx
031200061130     Area : xxx-xxxxxxxxxxxxxxxxxxxx                   Distretto :   4
031300061130------------------------------------- C  L  I  E  N  T  E --------   5
031400061130------------------- | ------------------------------------ C  L  I   6
031500061130  E  N  T  E ------------------------------------                    7
031600061130Codice    Ragione Sociale                          Partita IVA       8
031700061130XXXXXXX - XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX xxxxxxxxxxxxxxx   9
031800061130  Codice Fiscale    | Partita IVA      Codice Fiscale    Codice     10
031900061130x XXXXXXXXXXXXXXXX    XXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXX  XXXXXXX -
032000061130 Ragione Sociale                                                    12
032100061130 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                    2
032200061130                 ***   F I N E    S T A M P A  ***
