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
006100120424     c                   eval      V1Cfile = 'GAITRAAZP/WFDPD00F'
006200120424     c                   else
006300120424     c                   eval      V1Cfile = 'GAITRAAZM/WFDPD00F'
006400120424     c                   endif
006500020423      *
006600020423do  1c                   do        *hival
006700020423      *
006800020423      * Emissione VIDEO1
006900120424     c                   write     FI63T01
007000120424     c                   write     FI63Z01
007100020423     c                   exfmt     FI63D01
007200020423     c                   setoff                                       2890
007300120424sel 2c                   select
007400020423      * F3=Fine
007500120424w   2c                   when      *INKC
007600020423     c                   leave
007700120424      * F9=Visualizzazione WFDPD00F
007800120424if  2c                   when      *INKI
007900120424     c                   eval      Qcmd = 'DSPDBF file(*libl/WFDPD00F) +
008000120424     c                                            mbr(*first) output(*)'
008100120424     c                   exsr      sr_ExecCmd
008200120424     c                   iter
008300120424e   2c                   endsl
008400020423      * Controlli videata
008500020423     c                   exsr      CtrD01
008600020423      * se è acceso il 28 ritorno ad emettere il formato video
008700020423if  2c                   if        *in28 or *in90
008800020423     c                   iter
008900020423e   2c                   endif
009000020423      *
009100020423      * se premuto F6 lancio il pgm.
009200020423if  2c                   if        *INKF
009300020423     c                   exsr      SBMJOB
009400020423     c                   leave
009500020423x   2c                   else
009600020423      * ...altrimenti riemetto la videata
009700020423     c                   iter
009800020423e   2c                   endif
009900020423      *
010000020423e   1c                   enddo
010100020423      *
010200020620     c     Fine          tag
010300020423     c                   eval      *INLR = *on
010400020423
010500020423      ****************************************************************
010600020423      * CONTROLLI VIDEO 1
010700020423      ****************************************************************
010800020423     c     CtrD01        BEGSR
010900020423      *
011000020423sel 1c                   select
011100020423      * controllo data immessa se richiesta ristampa
011200020423wh  1c                   when          V1Cris =  'SI'
011300020423     c                             and V1Cdat <> *zeros
011400020423     c                   clear                   WLBDAT
011500020423     c                   eval      G02dat = V1Cdat
011600020423     c                   call      'XSRDA8'
011700020423     c                   parm                    WLBDAT
011800020423if  2c                   if        G02err = '1'
011900020423     c                   seton                                        28  51
012000020423x   2c                   else
012100020423     c                   eval      V1Cdat = G02dat
012200020423e   2c                   endif
012300020423      * data richiesta se richiesta ristampa
012400020423wh  1c                   when          V1Cris =  'SI'
012500020423     c                             and V1Cdat =  *zeros
012600020423     c                   seton                                        28  52
012700020423      * data non richiesta se non richiesta ristampa
012800020423wh  1c                   when          V1Cris <> 'SI'
012900020423     c                             and V1Cdat <> *zeros
013000020423     c                   seton                                        28  53
013100020423      *
013200020423e   1c                   endsl
013300020423      *
013400020423     c     EctrD01       ENDSR
013500020423
013600020423      ****************************************************************
013700020423      * SOTTOMISSIONE LAVORO
013800020423      ****************************************************************
013900020423     c     SBMJOB        BEGSR
014000020423      *
014100020423     c                   clear                   FIDN63DS
014200020423     c                   movel     V1Cris        I63ris
014300020423if  1c                   if        V1Cris = 'SI'
014400020423     c                   z-add     G02inv        I63dat
014500020423e   1c                   endif
014600120424     c                   movel     V1Cele        I63ele
014700020423     c                   movel(p)  FIDN63DS      KPJBU
014800020423      *
014900020423     c                   movel     'DN64'        KCOAZ
015000050711     ***c                   call      'FIDN64R'
015100050711     ***c                   call      'BCH10'
015200050711     c                   movel(p)  'BCH10'       Wpgm             10
015300050711     c                   call      Wpgm
015400020423     c                   parm                    KPJBA
015500020423      *
015600020423     c                   ENDSR
015700020423
015800020423      ****************************************************************
015900020423      * ROUTINE INIZIALE
016000020423      ****************************************************************
016100020423     c     *INZSR        BEGSR
016200020423      *
016300020423     c     *ENTRY        PLIST
016400020423     c                   PARM                    KPJBA
016500020423      *
016600050711      * Reperisco nome azienda per stampa PREPAGATI
016700050711     c     *dtaara       define    §azute        AZUTEds
016800050711     c     *dtaara       define    §datiute      dDATIUTE
016900050711     c                   in(E)     *dtaara
017000050711     c                   if        %ERROR or RSUT = *blanks
017100050711     c                   clear                   Tibs34Ds
017200050711     c                   call      'TIBS34R'
017300050711     c                   parm                    Tibs34Ds
017400050711     c                   in        *dtaara
017500050711     c                   endif
017600020423      *
017700020423     c                   ENDSR
017800120424      /free
017900120424       //--------------------------------------------------------------
018000120424       //?Esecuzione del comando (già impostato).                      ?
018100120424       //--------------------------------------------------------------
018200120424       BEGSR  sr_ExecCmd;
018300120424
018400120424         clear Qcap0100;
018500120424         Qcabcsdh = *off;
018600120424         Qcapa    = *off;
018700120424         Qcacmdss = *off;
018800120424         Qcaerved = *allX'00';
018900120424
019000120424         clear Qusec;
019100120424         Qusbprv  = %size(Qusec);
019200120424
019300120424         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
019400120424                           %size(Qcap0100) : 'CPOP0100' : *omit :
019500120424                           0 : 0 : Qusec);
019600120424
019700120424         //if  Qusei <> *blank;
019800120424         //  ...;
019900120424         //endif;
020000120424
020100120424       ENDSR;
020200120424      /end-free
