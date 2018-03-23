000100120424     /*PRM  dbgview(*source)
000200120424     /*END
000300020423      *--------------------------------------------------------------*
000400020423      *    Elenco C.A. per rivalsa DPD - filtro di lancio            *
000500020423      *--------------------------------------------------------------*
000600020423      *
000700020423      ****************************************************************
000800020423      *  RIEPILOGO INDICATORI
000900020423      ****************************************************************
001000020423      * 28 - ERRORE GENERICO DSPF
001100020423      * 50 - ERRORE Data errata
001200020423      * 51 - ERRORE Data obbligatoria in caso di RISTAMPA
001300020423      * 52 - ERRORE Data non richiesta in caso di 1a STAMPA
001400020423      * 90 - ERRORE GENERICO
001500020423      ***************************************************************
001600020423
001700020423     H DECEDIT('0,') DATEDIT(*DMY.)
001800020423
001900020423     FFIDN63D   CF   E             WORKSTN
002000020423
002100020423     D KPJBA         e ds
002200020423     D FIDN63DS      e ds                  inz
002300020423      *
002400050711      * - Parametri x Controllo profilo utenti
002500050711     d TIBS34ds      e ds
002600050711      * - Ds di riferimento al file esterno AZUTE00F
002700050711     d AZUTEds       e ds                  extname(AZUTE00F)
002800050711      * - Ds per dati organigramma
002900050711     d dDATIUTE      e ds
003000020423      *
003100020423     D STATUS         sds
003200020423     D  VTCPGM           *proc
003300020423      *
003400020423     D WLBDAT          ds                  inz
003500020423     D  G02DAT                 1      8  0
003600020423     D  G02INV                 9     16  0
003700020423     D  G02ERR                17     17
003800020423     D  G02TGI                18     22  0
003900020423      *
004000020423     D $FINE           s              1    inz(*off)
004100120424
004200120424       //--------------------------------------------------------------
004300120424       //?Definizione prototipi procedure.                             ?
004400120424       //--------------------------------------------------------------
004500120424
004600120424       // -?Parametri API QCAPCMD (Process Commands)?
004700120424     d Qcmd            s           2048    inz  varying
004800120424      /copy qSysInc/qRpgleSrc,QCAPCMD
004900120424       // -?API QCAPCMD (Process Commands)?
005000120424      /copy gaitrasrc/srcProtoPR,QCAPCMD
005100120424
005200120424       // -?Parametri gestione errori API.?
005300120424      /copy qsysinc/qrpglesrc,QUSEC
005400020423
005500020423      *--------------------------------------------------------------*
005600020423      *                   C   I   C   L   O                          *
005700020423      *--------------------------------------------------------------*
005800020423
005900120424     c                   eval      V1Cele = 'F'
006000120424     c                   if        %subst(knsif:7:1)='P'
006100150213     c*//                eval      V1Cfile = 'GAITRAAZP/WFDPD00F'
006200150213     c                   eval      V1Cfile = 'GAITRAAZP/WFDPD10F'
006300120424     c                   else
006400150213     c*//                eval      V1Cfile = 'GAITRAAZM/WFDPD00F'
006500150213     c                   eval      V1Cfile = 'GAITRAAZM/WFDPD10F'
006600120424     c                   endif
006700020423      *
006800020423do  1c                   do        *hival
006900020423      *
007000020423      * Emissione VIDEO1
007100120424     c                   write     FI63T01
007200120424     c                   write     FI63Z01
007300020423     c                   exfmt     FI63D01
007400020423     c                   setoff                                       2890
007500120424sel 2c                   select
007600020423      * F3=Fine
007700120424w   2c                   when      *INKC
007800020423     c                   leave
007900120424      * F9=Visualizzazione WFDPD00F
008000120424if  2c                   when      *INKI
008100150213     c*//                eval      Qcmd = 'DSPDBF file(*libl/WFDPD00F) +
008200150213     c                   eval      Qcmd = 'DSPDBF file(*libl/WFDPD10F) +
008300120424     c                                            mbr(*first) output(*)'
008400120424     c                   exsr      sr_ExecCmd
008500120424     c                   iter
008600120424e   2c                   endsl
008700020423      * Controlli videata
008800020423     c                   exsr      CtrD01
008900020423      * se è acceso il 28 ritorno ad emettere il formato video
009000020423if  2c                   if        *in28 or *in90
009100020423     c                   iter
009200020423e   2c                   endif
009300020423      *
009400020423      * se premuto F6 lancio il pgm.
009500020423if  2c                   if        *INKF
009600020423     c                   exsr      SBMJOB
009700020423     c                   leave
009800020423x   2c                   else
009900020423      * ...altrimenti riemetto la videata
010000020423     c                   iter
010100020423e   2c                   endif
010200020423      *
010300020423e   1c                   enddo
010400020423      *
010500020620     c     Fine          tag
010600020423     c                   eval      *INLR = *on
010700020423
010800020423      ****************************************************************
010900020423      * CONTROLLI VIDEO 1
011000020423      ****************************************************************
011100020423     c     CtrD01        BEGSR
011200020423      *
011300020423sel 1c                   select
011400020423      * controllo data immessa se richiesta ristampa
011500020423wh  1c                   when          V1Cris =  'SI'
011600020423     c                             and V1Cdat <> *zeros
011700020423     c                   clear                   WLBDAT
011800020423     c                   eval      G02dat = V1Cdat
011900020423     c                   call      'XSRDA8'
012000020423     c                   parm                    WLBDAT
012100020423if  2c                   if        G02err = '1'
012200020423     c                   seton                                        28  51
012300020423x   2c                   else
012400020423     c                   eval      V1Cdat = G02dat
012500020423e   2c                   endif
012600020423      * data richiesta se richiesta ristampa
012700020423wh  1c                   when          V1Cris =  'SI'
012800020423     c                             and V1Cdat =  *zeros
012900020423     c                   seton                                        28  52
013000020423      * data non richiesta se non richiesta ristampa
013100020423wh  1c                   when          V1Cris <> 'SI'
013200020423     c                             and V1Cdat <> *zeros
013300020423     c                   seton                                        28  53
013400020423      *
013500020423e   1c                   endsl
013600020423      *
013700020423     c     EctrD01       ENDSR
013800020423
013900020423      ****************************************************************
014000020423      * SOTTOMISSIONE LAVORO
014100020423      ****************************************************************
014200020423     c     SBMJOB        BEGSR
014300020423      *
014400020423     c                   clear                   FIDN63DS
014500020423     c                   movel     V1Cris        I63ris
014600020423if  1c                   if        V1Cris = 'SI'
014700020423     c                   z-add     G02inv        I63dat
014800020423e   1c                   endif
014900120424     c                   movel     V1Cele        I63ele
015000020423     c                   movel(p)  FIDN63DS      KPJBU
015100020423      *
015200020423     c                   movel     'DN64'        KCOAZ
015300050711     ***c                   call      'FIDN64R'
015400050711     ***c                   call      'BCH10'
015500050711     c                   movel(p)  'BCH10'       Wpgm             10
015600050711     c                   call      Wpgm
015700020423     c                   parm                    KPJBA
015800020423      *
015900020423     c                   ENDSR
016000020423
016100020423      ****************************************************************
016200020423      * ROUTINE INIZIALE
016300020423      ****************************************************************
016400020423     c     *INZSR        BEGSR
016500020423      *
016600020423     c     *ENTRY        PLIST
016700020423     c                   PARM                    KPJBA
016800020423      *
016900050711      * Reperisco nome azienda per stampa PREPAGATI
017000050711     c     *dtaara       define    §azute        AZUTEds
017100050711     c     *dtaara       define    §datiute      dDATIUTE
017200050711     c                   in(E)     *dtaara
017300050711     c                   if        %ERROR or RSUT = *blanks
017400050711     c                   clear                   Tibs34Ds
017500050711     c                   call      'TIBS34R'
017600050711     c                   parm                    Tibs34Ds
017700050711     c                   in        *dtaara
017800050711     c                   endif
017900020423      *
018000020423     c                   ENDSR
018100120424      /free
018200120424       //--------------------------------------------------------------
018300120424       //?Esecuzione del comando (già impostato).                      ?
018400120424       //--------------------------------------------------------------
018500120424       BEGSR  sr_ExecCmd;
018600120424
018700120424         clear Qcap0100;
018800120424         Qcabcsdh = *off;
018900120424         Qcapa    = *off;
019000120424         Qcacmdss = *off;
019100120424         Qcaerved = *allX'00';
019200120424
019300120424         clear Qusec;
019400120424         Qusbprv  = %size(Qusec);
019500120424
019600120424         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
019700120424                           %size(Qcap0100) : 'CPOP0100' : *omit :
019800120424                           0 : 0 : Qusec);
019900120424
020000120424         //if  Qusei <> *blank;
020100120424         //  ...;
020200120424         //endif;
020300120424
020400120424       ENDSR;
020500120424      /end-free
