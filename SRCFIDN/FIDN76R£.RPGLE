000100120423     /*PRM  dbgview(*source)
000200120423     /*CMD  ovrdbf file(WFDEX00F) tofile(GAITRAAZP/WFDEX00F) +
000300120423     /*CMD         ovrscope(*calllvl)
000400120423     /*END  dltovr file(WFDEX00F) lvl(*)
000500120423     /*END
000600020423      *--------------------------------------------------------------*
000700110728      *    Elenco C.A. per rivalsa EEX - filtro di lancio            *
000800020423      *--------------------------------------------------------------*
000900120423
001000020423      ****************************************************************
001100020423      *  RIEPILOGO INDICATORI
001200020423      ****************************************************************
001300020423      * 28 - ERRORE GENERICO DSPF
001400020423      * 50 - ERRORE Data errata
001500020423      * 51 - ERRORE Data obbligatoria in caso di RISTAMPA
001600020423      * 52 - ERRORE Data non richiesta in caso di 1a STAMPA
001700020423      * 90 - ERRORE GENERICO
001800020423      ***************************************************************
001900020423
002000020423     H DECEDIT('0,') DATEDIT(*DMY.)
002100020423
002200110728     FFIDN76D   CF   E             WORKSTN
002300020423
002400020423     D KPJBA         e ds
002500110728     D FIDN76DS      e ds                  inz
002600020423      *
002700050711      * - Parametri x Controllo profilo utenti
002800050711     d TIBS34ds      e ds
002900050711      * - Ds di riferimento al file esterno AZUTE00F
003000050711     d AZUTEds       e ds                  extname(AZUTE00F)
003100050711      * - Ds per dati organigramma
003200050711     d dDATIUTE      e ds
003300020423      *
003400020423     D STATUS         sds
003500020423     D  VTCPGM           *proc
003600020423      *
003700020423     D WLBDAT          ds                  inz
003800020423     D  G02DAT                 1      8  0
003900020423     D  G02INV                 9     16  0
004000020423     D  G02ERR                17     17
004100020423     D  G02TGI                18     22  0
004200020423      *
004300020423     D $FINE           s              1    inz(*off)
004400120423
004500120423       //--------------------------------------------------------------
004600120423       //?Definizione prototipi procedure.                             ?
004700120423       //--------------------------------------------------------------
004800120423
004900120423       // -?Parametri API QCAPCMD (Process Commands)?
005000120423     d Qcmd            s           2048    inz  varying
005100120423      /copy qSysInc/qRpgleSrc,QCAPCMD
005200120423       // -?API QCAPCMD (Process Commands)?
005300120423      /copy gaitrasrc/srcProtoPR,QCAPCMD
005400120423
005500120423       // -?Parametri gestione errori API.?
005600120423      /copy qsysinc/qrpglesrc,QUSEC
005700020423
005800020423      *--------------------------------------------------------------*
005900020423      *                   C   I   C   L   O                          *
006000020423      *--------------------------------------------------------------*
006100020423
006200120423     c                   eval      V1Cele = 'F'
006300120423     c                   if        %subst(knsif:7:1)='P'
006400120423     c                   eval      V1Cfile = 'GAITRAAZP/WFDEX00F'
006500120423     c                   else
006600120423     c                   eval      V1Cfile = 'GAITRAAZM/WFDEX00F'
006700120423     c                   endif
006800020423      *
006900020423do  1c                   do        *hival
007000020423      *
007100020423      * Emissione VIDEO1
007200120424     c                   write     FI76T01
007300120424     c                   write     FI76Z01
007400110728     c                   exfmt     FI76D01
007500020423     c                   setoff                                       2890
007600120423     c                   select
007700020423      * F3=Fine
007800120423if  2c                   when      *INKC
007900020423     c                   leave
008000120423      * F9=Visualizzazione WFDEX00F
008100120423if  2c                   when      *INKI
008200120423     c                   eval      Qcmd = 'DSPDBF file(*libl/WFDEX00F) +
008300120423     c                                            mbr(*first) output(*)'
008400120423     c                   exsr      sr_ExecCmd
008500120423     c                   iter
008600120423e   2c                   endsl
008700120423      *
008800020423      * Controlli videata
008900020423     c                   exsr      CtrD01
009000020423      * se è acceso il 28 ritorno ad emettere il formato video
009100020423if  2c                   if        *in28 or *in90
009200020423     c                   iter
009300020423e   2c                   endif
009400020423      *
009500020423      * se premuto F6 lancio il pgm.
009600020423if  2c                   if        *INKF
009700020423     c                   exsr      SBMJOB
009800020423     c                   leave
009900020423x   2c                   else
010000020423      * ...altrimenti riemetto la videata
010100020423     c                   iter
010200020423e   2c                   endif
010300020423      *
010400020423e   1c                   enddo
010500020423      *
010600020620     c     Fine          tag
010700020423     c                   eval      *INLR = *on
010800020423
010900020423      ****************************************************************
011000020423      * CONTROLLI VIDEO 1
011100020423      ****************************************************************
011200020423     c     CtrD01        BEGSR
011300020423      *
011400020423sel 1c                   select
011500020423      * controllo data immessa se richiesta ristampa
011600020423wh  1c                   when          V1Cris =  'SI'
011700020423     c                             and V1Cdat <> *zeros
011800020423     c                   clear                   WLBDAT
011900020423     c                   eval      G02dat = V1Cdat
012000020423     c                   call      'XSRDA8'
012100020423     c                   parm                    WLBDAT
012200020423if  2c                   if        G02err = '1'
012300020423     c                   seton                                        28  51
012400020423x   2c                   else
012500020423     c                   eval      V1Cdat = G02dat
012600020423e   2c                   endif
012700020423      * data richiesta se richiesta ristampa
012800020423wh  1c                   when          V1Cris =  'SI'
012900020423     c                             and V1Cdat =  *zeros
013000020423     c                   seton                                        28  52
013100020423      * data non richiesta se non richiesta ristampa
013200020423wh  1c                   when          V1Cris <> 'SI'
013300020423     c                             and V1Cdat <> *zeros
013400020423     c                   seton                                        28  53
013500020423      *
013600020423e   1c                   endsl
013700020423      *
013800020423     c     EctrD01       ENDSR
013900020423
014000020423      ****************************************************************
014100020423      * SOTTOMISSIONE LAVORO
014200020423      ****************************************************************
014300020423     c     SBMJOB        BEGSR
014400020423      *
014500110728     c                   clear                   FIDN76DS
014600110728     c                   movel     V1Cris        I76ris
014700020423if  1c                   if        V1Cris = 'SI'
014800110728     c                   z-add     G02inv        I76dat
014900020423e   1c                   endif
015000120423     c                   movel     V1Cele        I76ele
015100110728     c                   movel(p)  FIDN76DS      KPJBU
015200020423      *
015300110728     c                   movel     'DN77'        KCOAZ
015400110801     c                   movel(p)  'BCH10'       Wpgm             10
015500110801     c****               movel(p)  'FIDN77C'     Wpgm             10
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
018100120423      /free
018200120423       //--------------------------------------------------------------
018300120423       //?Esecuzione del comando (già impostato).                      ?
018400120423       //--------------------------------------------------------------
018500120423       BEGSR  sr_ExecCmd;
018600120423
018700120423         clear Qcap0100;
018800120423         Qcabcsdh = *off;
018900120423         Qcapa    = *off;
019000120423         Qcacmdss = *off;
019100120423         Qcaerved = *allX'00';
019200120423
019300120423         clear Qusec;
019400120423         Qusbprv  = %size(Qusec);
019500120423
019600120423         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
019700120423                           %size(Qcap0100) : 'CPOP0100' : *omit :
019800120423                           0 : 0 : Qusec);
019900120423
020000120423         //if  Qusei <> *blank;
020100120423         //  ...;
020200120423         //endif;
020300120423
020400120423       ENDSR;
020500120423      /end-free
