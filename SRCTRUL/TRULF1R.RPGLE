000100020208     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200020208      *--------------------------------------------------------------------------------------------*
000300161220      *     controllo linee fogli viaggio
000400020208      *--------------------------------------------------------------------------------------------*
000500020208     FAzOrg01l  IF   E           K DISK
000600161220     Ftrulf1d   CF   E             WORKSTN
000700020208      *--------------------------------------------------------------------------------------------*
000800020208      * Data structure                                                                             *
000900020208      *--------------------------------------------------------------------------------------------*
001000020208      *---
001100020208      * Schiere memorizzazione dati correnti
001200020208      *---
001300020320     D msg             S             78    DIM(6) CTDATA PERRCD(1)              *Messaggi video
001400120418     D CMDchk          S             49    DIM(1) CTDATA PERRCD(1)              QCAEXEC
001500070126     d
001600120418     D Dcur          E DS                                                       *ds di procedura
001700020208      *
001800020208     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001900120418     D fnlv55ds      E DS
002000120419     D tibs02ds      E DS
002100020208     D kpjba         E DS                                                       *architettura
002200120420     D dsemail       E DS
002300120418     D***
002400120418     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
002500120418     D***
002600120418     D WLBDAT          DS                  INZ
002700120418     D  G02DAT                 1      8  0
002800120418     D  G02INV                 9     16  0
002900120418     D  G02ERR                17     17
003000120418     D  G02TGI                18     22  0
003100161220
003200161220     D fnls29ds        DS
003300161220     d* Terminal di partenza
003400161220     D  I29TFP                 1      3  0
003500161220     c* Numero foglio viaggio: se = 0 tutti i fogli aperti del terminal
003600161220     D  I29nfv                 4      9  0
003700161220     D  o29err                10     10
003800161220     D  I29tfa                11     13  0
003900161220     D  I29lna                14     16  0
004000161220     D  I29azione             17     17
004100161220     D  I29mail               18     42
004200161220
004300120420     d
004400161220     d codfil          s              3  0
004500120418     d Dataiso         s               d   datfmt(*iso)
004600120418     d Dataeur         s               d   datfmt(*eur)
004700020208      *--------------------------------------------------------------------------------------------*
004800020208      * Main lines
004900020208      *--------------------------------------------------------------------------------------------*
005000020208      *
005100020208      * Operazioni iniziali
005200020208      *
005300020208     C                   EXSR      RutInz
005400020208      *
005500020208      * Gestione video
005600020208      *
005700020208     C     $Fine         DOUEQ     'S'
005800161220     c                   exsr      Gesvd1
005900020208     C                   ENDDO
006000020208      *
006100020208     C                   SETON                                        LR
006200020208      *--------------------------------------------------------------------------------------------*
006300020208      * GesVd1 - Gestione videata immissione nuova tabella -
006400020208      *--------------------------------------------------------------------------------------------*
006500020208     C     GesVd1        BEGSR
006600020208      *
006700020208      * Inizializzazione videata
006800120420     c                   clear                   v1cemail
006900161220     c                   clear                   v1ctfp
007000161220     c                   clear                   v1ctfa
007100161220     c                   clear                   v1clna
007200161220     c                   clear                   of1msg
007300020208      *
007400070126     c     for01         tag
007500161220     C                   EXFMT     ulf1v1
007600070126     c                   clear                   v1cmsg
007700161220     c                   clear                   of1msg
007800120420     c
007900070126     c                   setoff                                       2890
008000120420     c                   setoff                                       4041
008100020208      *
008200070126     c* F3 -  fine
008300070126     c                   if        *inkc
008400070126     C                   EXSR      F03Ges
008500161220     c                   leavesr
008600070126     c                   endif
008700120420
008800161220     c                   exsr      Ctrvid
008900120420     c
009000120420     c                   if        *in90
009100120420     c                   goto      for01
009200120420     c                   endif
009300161220
009400161220     c  nkf              goto      for01
009500161220
009600161220     c* F6 - elabora controlli
009700161220     c                   if        *inkf
009800161220     c                   clear                   fnls29ds
009900161220     c                   eval      i29mail=v1cemail
010000161220     c                   eval      i29tfp=v1ctfp
010100161220     c                   eval      i29tfa=v1ctfa
010200161220     c                   eval      i29lna=v1clna
010300161220     c                   if        v1clna>0 or v1ctfa>0
010400161220     c                   eval      i29azione='E'
010500161220     c                   endif
010600161220     c                   clear                   of1msg
010700161220     c                   movel     fnls29ds      kpjbu
010800161220     c                   call      'TRULF2R'
010900161220     c                   parm                    kpjba
011000161220     c                   parm                    of1msg
011100161220     c
011200161220     c* riemetto campi messaggi pieno
011300161220     c                   if        of1msg=*blanks
011400161220     c                   eval      of1msg='tutto OK'
011500161220     c                   endif
011600161220     c                   goto      for01
011700161220     c                   endif
011800120420     c
011900020208      *
012000161220     C                   endsr
012100070126     C**************************************************************************
012200070126     C*  CONTROLLO UNA FILIALE IN ORGANIGRAMMA
012300070126     C**************************************************************************
012400070126     C     CTRFIL        BEGSR
012500070126
012600070126     C     CODFIL        CHAIN     AZORG01L                           34
012700070126    1C  N34ORGFVA        IFNE      ' '
012800070126     C     ORGFAG        ORNE      'A'
012900070126     C     ORGFAG        ANDNE     'F'
013000070126     C                   SETON                                        34
013100070126    1C                   ENDIF
013200070126     c
013300070126    1c                   if        not *in34
013400070126     c
013500070126   x1C                   ELSE
013600120418     c                   seton                                        419028
013700070126     C                   MOVEL     MSG(1)        V1cMSG
013800070126     c                   goto      endctf
013900070126    1C                   ENDIF
014000070126     C*
014100070126     C     endctf        ENDSR
014200020208      *--------------------------------------------------------------------------------------------*
014300020208      * F03ges - Tasto funzionale f03 -> Fine programma
014400020208      *--------------------------------------------------------------------------------------------*
014500020208     C     F03ges        BEGSR
014600070126     C                   movel     'S'           $Fine             1            *fine pgm
014700020208     C                   ENDSR
014800120420      *--------------------------------------------------------------------------------------------*
014900161220      * controllo indirizzo e-mail e altri campi video
015000120420      *--------------------------------------------------------------------------------------------*
015100161220     C     Ctrvid        BEGSR
015200161220     c* terminal partenza
015300161220     c                   if        v1ctfp>0
015400161220     c                   eval      codfil=v1ctfp
015500161220     c                   exsr      ctrfil
015600161220     c                   if        *in90
015700161220     c                   leavesr
015800161220     c                   endif
015900161220     c                   endif
016000161220     c* terminal arrivo
016100161220     c                   if        v1ctfa>0
016200161220     c                   eval      codfil=v1ctfa
016300161220     c                   exsr      ctrfil
016400161220     c                   if        *in90
016500161220     c                   leavesr
016600161220     c                   endif
016700161220     c                   endif
016800161220     c* linea di arrivo
016900161220     c                   if        v1clna>0
017000161220     c                   eval      codfil=v1clna
017100161220     c                   exsr      ctrfil
017200161220     c                   if        *in90
017300161220     c                   leavesr
017400161220     c                   endif
017500161220     c                   endif
017600120420     c
017700120420     C                   if        v1cemail<>*blanks
017800120420     c                   clear                   dsemail
017900120420     c                   eval      §EMLINDI=%trim(v1cemail)+'@brt.it'
018000120420     c                   call      'XEMAIL'
018100120420     c                   parm                    dsemail
018200120420     c
018300120420     c                   if        §EMLERRO<>*blanks and §EMLERRO<> '0'
018400120420     c                   seton                                        412890
018500120420     C                   MOVEL     §EMLMSGO      V1cMSG
018600120420     c                   leavesr
018700120420     c                   endif
018800120420     c
018900120420     c                   if        v1cemail=*blanks
019000120420     c                   seton                                        412890
019100120420     C                   MOVEL     msg(5)        V1cMSG
019200120420     c                   leavesr
019300120420     c                   endif
019400161220     c                   endif
019500120420     c
019600120420     C                   ENDSR
019700020208      *--------------------------------------------------------------------------------------------*
019800020208      * RutInz - operazioni iniziali
019900020208      *--------------------------------------------------------------------------------------------*
020000020208     C     rutinz        BEGSR
020100020208      *
020200020208      * Ricevimento parametri
020300020208     C     *ENTRY        PLIST
020400020208     C                   PARM                    kpjba
020500020208      *
020600020208     C                   z-add     1             CodUt
020700020208     C                   CALL      'X§PARUT'
020800020208     C                   PARM                    UT§DSE
020900020211      *
021000070126     c
021100070126     c                   movel     ragut         v1crag
021200070126     c                   movel     knmus         v1cute
021300070126     c                   movel     knsif         v1csif
021400070126     c                   ENDSR
021500020212      *
021600020211      *
021700020212** Msg
021800120418Filiale inesistente o errata
021900120418La libreria deve essere una GAICURxxx esistente
022000120418data errata
022100120418La filiale immessa non è un terminal di partenza alla data indicata
022200161220immettere indirizzo e-amil BRT
