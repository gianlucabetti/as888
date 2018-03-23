000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200060517      * TNTA73R *----------------------------------------------------*
000300061127      *        Aggiorna codici fiscali su codici clienti
000400061127      *              collegati con la partita iva
000500940726      *--------------------------------------------------------------*
000600061127     fCnAco00f  uf   e           k disk
000700061127     fCnind02l  uf   e           k disk
000800061127     fcnind03l  if   e           k disk    rename(cnind000:cnind003)
000900061128     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001000061128     F                                     USROPN
001100061128     F                                     FORMLEN(66)
001200061128     F                                     FORMOFL(63)
001300061128     D CMDovr          S             80    DIM(1) CTDATA PERRCD(1)
001400061128     D TE              S             66    DIM(20) CTDATA PERRCD(1)
001500061127     d
001600061127     d kpjba         e ds
001700061127     d Tibs34ds      e ds
001800061127     d dDatiute      e ds
001900061127     d Azuteds       e ds                  Extname(Azute00f)
002000061128     d
002100061128     d tnta74ds        ds
002200061128     d  ta74aggior             1      1
002300061128     d  ta74stampa             2      2
002400061127     D Kut             S                   LIKE(indkut) inz(1)
002500061127     D Kcdf            S                   LIKE(indcdf) inz
002600061127     D aggcdf          S                   LIKE(indcdf)
002700061128     D aggksc          S                   LIKE(indksc)
002800061127     D Kiva            S                   LIKE(indiva)
002900021212      *--------------------------------------------------------------*     *****
003000021212     c     *ENTRY        PLIST
003100060523     c                   PARM                    kpjba
003200061128     c                   movel     kpjbu         tnta74ds
003300061128     c* Senza aggiornamento
003400061128     c                   if        ta74aggior<>'S'
003500061128     c                   seton                                        05
003600061128     c                   endif
003700061128     c
003800061128     c* Se prevede stampa, apro il file printer
003900061128     c                   if        ta74stampa='S'
004000061128     C                   MOVEA     CMDovr        COMMAN           80
004100061128     c                   z-add     80            lung
004200061128     C*
004300061128     C                   CALL      'QCMDEXC'                              98
004400061128     C                   PARM                    COMMAN           80
004500061128     C                   PARM                    LUNG             15 5
004600061128     C  N98              OPEN      PRTF198
004700061128     C
004800061128     C                   EXCEPT    TESTA
004900061128     c                   endif
005000061128     c
005100061128     C* Se non devo aggornare e nemmeno stampare, esco dal pgm
005200061128    0c                   if        ta74aggior='S' or ta74stampa='S'
005300061128     c
005400061128     c* se non previsto aggiornamento accendo un indicatore
005500061127     c
005600061127     c* leggo la lf  dei codici fiscali con cdf<>*blanks
005700061127     c     kind03a       setgt     cnind03l
005800061127     c     kind03b       reade     cnind03l
005900061127     c
006000061127    1c                   dow       not %eof(cnind03l)
006100061127     c* Escludo la partiva iva : PRIVATO  o iso = $$
006200061127     c*                          vuota o = 00000000000
006300061127   1ac                   if        indiva<>*blanks and indiva<>'00000000000    '
006400061127     c                             and %subst(indiva:1:2)<>'$$'
006500061127     C     'PRIVATO'     SCAN      indiva                                 30
006600061127   1bc                   if        not *in30
006700061128     c                   setoff                                       06
006800061128     c
006900061127     c* Mi salvo i dati necessari e verifico se ci sono altri
007000061127     c*  codici con la stessa p.iva
007100061127     c                   eval      kiva=indiva
007200061127     c                   eval      aggcdf=indcdf
007300061128     c                   eval      aggKSC=indksc
007400061128     c     kaco          chain(N)  cnaco000
007500061128     c                   if        %found(cnaco00f)
007600061128     c                   movel     acorag        aggrag           40
007700061128     c                   else
007800061128     c                   eval      aggrag=*all'?'
007900061128     c                   endif
008000061127     c
008100061127     c     kind02a       setll     cnind02l
008200061128     c
008300061128     c                   if        ta74aggior='S'
008400061127     c     kind02a       reade     cnind02l
008500061128     c                   else
008600061128     c     kind02a       reade(N)  cnind02l
008700061128     c                   endif
008800061128     c
008900061127    2c                   dow       not %eof(cnind02l)
009000061128     c
009100061127     c* Se non è il codice letto, aggiorno se vuota
009200061127    3c                   if        indcdf=*blanks
009300061127     c                   eval      indcdf=aggcdf
009400061128     c
009500061128    4c                   if        ta74aggior='S'
009600061128     c* chain su cnaco per sflaggare i dati
009700061127     c     kaco          chain     cnaco000
009800061128    5c                   if        %found(cnaco00f)
009900061127     c                   Clear                   acodtr
010000061127     c                   Clear                   acoftr
010100061127     c                   update    cnaco000
010200061128    5c                   endif
010300061127     c
010400061127     c                   update    cnind000
010500061128   x4c                   else
010600061128     c* per la stampa
010700061128     c     kaco          chain(N)  cnaco000
010800061128    4c                   endif
010900061128     c
011000061128    4c                   if        not %found(cnaco00f)
011100061128     c                   eval      acorag=*all'?'
011200061128    4c                   endif
011300061128     c
011400061128     c* Se prevista la stampa, eseguo
011500061128    4c                   if        ta74stampa='S' and %open(prtf198)
011600061128     c                   if        *inof
011700061128     c                   except    testa
011800061128     c                   setoff                                       06
011900061128     c                   endif
012000061128     c
012100061128     c                   except    detta
012200061128     c                   seton                                        06
012300061128     c
012400061128    4c                   endif
012500061128    3c                   endif
012600061127     c
012700061128    4c                   if        ta74aggior='S'
012800061127     c     kind02a       reade     cnind02l
012900061128     c                   else
013000061128     c     kind02a       reade(N)  cnind02l
013100061128     c                   endif
013200061128     c
013300061127    2c                   enddo
013400061127   1bc                   endif
013500061127   1ac                   endif
013600061127     c
013700061127     c     kind03b       reade     cnind03l
013800061127    1c                   enddo
013900061128     c
014000061128    0c                   endif
014100061128    4c                   if        ta74stampa='S' and %open(prtf198)
014200061128     c                   except    fine
014300061128     c                   endif
014400021212      *
014500030122     C                   SETON                                        LR
014600061127      *------------------------------------------------------------------------*
014700061127      * ROUTINE INIZIALE
014800061127      *------------------------------------------------------------------------*
014900061127     c     *Inzsr        BegSr
015000061127
015100061127     c     *dtaara       define    §azute        azuteds
015200061127     c     *dtaara       define    §datiute      ddatiute
015300061127     c                   in(E)     *dtaara
015400061127     c                   If        %error  or rsut = *blanks
015500061127     c                   Clear                   Tibs34ds
015600061127     c                   Call      'TIBS34R'
015700061127     c                   Parm                    Tibs34ds
015800061127     c                   In        *dtaara
015900061127     c                   EndIf
016000061128     c
016100061128     c                   time                    wtime             6 0
016200061127     c
016300061127     c     kind03a       klist
016400061127     c                   kfld                    kut
016500061127     c                   kfld                    dutkci
016600061127     c                   kfld                    kcdf
016700061127     c     kind03b       klist
016800061127     c                   kfld                    kut
016900061127     c                   kfld                    dutkci
017000061127     c     kind02a       klist
017100061127     c                   kfld                    kut
017200061127     c                   kfld                    dutkci
017300061127     c                   kfld                    kiva
017400061127     c     kaco          klist
017500061127     c                   kfld                    kut
017600061127     c                   kfld                    indkcc
017700061127     c                   kfld                    indksc
017800061127     c
017900061127     c                   ENDSR
018000061128     OPRTF198   E            TESTA            02
018100061128     O                       TE(1)              132
018200061128     O                       TE(2)              198
018300061128     O                       RSUT                20
018400061128     O                       *DATE              180 '  /  /    '
018500061128     O                       PAGE          Z    198
018600061128     O          E            TESTA       1
018700061128     O                       KNSIF               10
018800061128     O                       KNMUS               22
018900061128     O                       wtime              179 '  :  :  '
019000061128     O          E            TESTA            04
019100061128     O               05      TE(3)               66
019200061128     O              N05      TE(4)               66
019300061128     O          E            TESTA            06
019400061128     O                       TE(5)               66
019500061128     O                       TE(6)              132
019600061128     O                       TE(7)              198
019700061128     O          E            TESTA            07 08
019800061128     O                       TE(8)               66
019900061128     O                       TE(10)             132
020000061128     O          E            DETTA       1
020100061128     o              n06      aggksc               7
020200061128     o              n06                           9 '-'
020300061128     o              n06      aggrag           +   1
020400061128     o              n06      kiva             +   1
020500061128     o              n06      aggcdf           +   1
020600061128     o                       indksc           +   4
020700061128     o                                        +   1 '-'
020800061128     o                       acorag           +   1
020900061128     O          E            FINE             65
021000061128     O                       TE(13)             132
021100061128** Comandi    567890123456789012345678901234567890123456789012345678901234567890
021200061128OVRPRTF FILE(PRTF198)  USRDTA('AggioCFisc') HOLD(*YES) OUTQ(LJ4200CED)
021300061128**
021400061128 ** AGGIORNAMENTO CODICE FISCALE SU CLIENTI CON STESSA PARTITA IVA   1
021500061128 **                    TNTA74R                           PAG. XXXX   2
021600061128S E N Z A   A G G I O R N A M E N T O                                3
021700061128C O N       A G G I O R N A M E N T O                                4
021800061128----------------------- C L I E N T E    C H E    A G G I O R N A    5
021900061128--------------------   ------------ C L I E N T E    A G G I O R N
022000061128 A T O -----------
022100061128CODICE    RAGIONE SOCIALE                          PARTITA IVA       8
022200061128XXXXXXX - XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX xxxxxxxxxxxxxxx   9
022300061128  CODICE FISCALE      CODICE    RAGIONE SOCIALE
022400061128x XXXXXXXXXXXXXXXX    XXXXXXX - xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
022500061128xxxxxxxxxxxxxx                                                       2
022600061128                 ***   F I N E    S T A M P A  ***
