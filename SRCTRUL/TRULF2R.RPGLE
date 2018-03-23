000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F*--------------------------------------------------------------*
000300121213     FAZCAE01L  IF   E           K DISK
000400020802     D
000500121211     D KPJBA         E DS
000600121211     D fnls29ds        DS
000700161220     D  I29TFP                 1      3  0
000800161220     D  I29nfv                 4      9  0
000900161220     D  o29err                10     10
001000161220     D  I29tfa                11     13  0
001100161220     D  I29lna                14     16  0
001200161220     D  I29azione             17     17
001300161220     D  I29mail               18     42
001400161220     d*
001500121213     d Dataiso         s               d   datfmt(*iso) inz(*sys)
001600161220     d***************************************************************
001700161220     c     *entry        plist
001800161220     c                   parm                    kpjba
001900161220     c                   parm                    of1msg          944
002000161220     c                   movel     kpjbu         fnls29ds
002100161220     c
002200121213     c                   movel     dataiso       dateu             8 0
002300161220     c                   clear                   indx              4 0
002400161220     c***
002500161220     c* e passato terminal di partenza, uso quello
002600161220    0c                   if        i29tfp>0
002700161220     C                   MOVEL     FNLS29DS      KPJBU
002800161220     c                   clear                   o29msg
002900161220     c                   call      'FNLS29R'
003000161220     c                   parm                    kpjbA
003100161220     c                   parm                    o29msg          256
003200161220     c
003300161220     c                   eval      of1msg='ter.Part ' + %editc(i29tfp:'X')+'-'
003400161220     c                   if        o29msg<>*blanks
003500161220     c                   eval      of1msg=%trim(of1msg)+o29msg
003600161220     c                   else
003700161220     c                   eval      of1msg=%trim(of1msg)+'tutto OK'
003800161220     c                   endif
003900161220
004000161220   x0c                   else
004100980528     c**
004200121213     C* SOLO SE MEMBRO PIENO
004300121213     C     'P'           SETLL     AZCAE01L
004400121213     C     'P'           READE     AZCAE01L                               31
004500121213     C*
004600121213    1C     *IN31         DOWEQ     *OFF
004700121213     C*
004800121213    2C     CAETFP        IFEQ      CAETFE
004900121213     C     CAEATB        ANDEQ     *BLANKS
005000121213     C     dateu         ANDGE     CAEDDE
005100121213     C     dateu         ANDLE     CAEDSC
005200121213     C*
005300161220     c                   clear                   o29msg
005400121213     C                   EVAL      I29TFP=caetfe
005500121211     C                   MOVEL     FNLS29DS      KPJBU
005600121211     c                   call      'FNLS29R'
005700121211     c                   parm                    kpjbA
005800161220     c                   parm                    o29msg
005900161220     c
006000161220    3c                   if        o29msg<>*blanks
006100161220
006200161220     c                   eval      of1msg=%trim(of1msg)+
006300161220     c                                     '/'+o29msg
006400161220     c                   eval      msgdet=%trim(msgdet)+
006500161220     c                                   '/'+ o29msg
006600161220    3c                   endif
006700161220
006800161220     c* Verifico quanti caratteri pieni
006900161220     c                   if        msgdet<>*blanks
007000161220     c                   eval      Indx=%checkr(' ':msgdet)
007100121213     c
007200161220    3c                   if        indx<30  and i29mail<>*blanks
007300161220     c                   eval      msgtes='Segnalazioni Fogli viaggio'
007400121213     C
007500161220     C                   eval      wrkEml= %trim(i29mail) +'@brt.it'
007600121213     C                   call(e)   'TIS701C'
007700121213     C                   parm                    wrkEml          253
007800121213     C                   PARM                    MSGTES           44
007900121213     C                   PARM                    MSGDET         5000
008000161220     c
008100161220     c                   clear                   msgdet
008200121213     C*
008300161220    3c                   endif
008400161220    3c                   endif
008500161220    2c                   endif
008600121213     C*
008700121213     C     'P'           READE     AZCAE01L                               31
008800161220    1c                   enddo
008900161220
009000161220    3c                   if        i29mail<>*blanks   and msgdet<>*blanks
009100161220     c                   eval      msgtes='Segnalazioni Fogli viaggio'
009200161220     C
009300161220     C                   eval      wrkEml= %trim(i29mail) +'@brt.it'
009400161220     C                   call(e)   'TIS701C'
009500161220     C                   parm                    wrkEml          253
009600161220     C                   PARM                    MSGTES           44
009700161220     C                   PARM                    MSGDET         5000
009800161220     c                   endif
009900161220     c
010000161220    0c                   endif
010100020322     C*
010200020322     C                   SETON                                        LR
