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
007200160818     c                   if        not *in28
007300120424     c                   write     FI76T01
007400120424     c                   write     FI76Z01
007500160818     c                   endif
007600110728     c                   exfmt     FI76D01
007700020423     c                   setoff                                       2890
007800120423     c                   select
007900020423      * F3=Fine
008000120423if  2c                   when      *INKC
008100020423     c                   leave
008200120423      * F9=Visualizzazione WFDEX00F
008300120423if  2c                   when      *INKI
008400120423     c                   eval      Qcmd = 'DSPDBF file(*libl/WFDEX00F) +
008500120423     c                                            mbr(*first) output(*)'
008600120423     c                   exsr      sr_ExecCmd
008700120423     c                   iter
008800120423e   2c                   endsl
008900120423      *
009000020423      * Controlli videata
009100020423     c                   exsr      CtrD01
009200020423      * se è acceso il 28 ritorno ad emettere il formato video
009300020423if  2c                   if        *in28 or *in90
009400020423     c                   iter
009500020423e   2c                   endif
009600020423      *
009700020423      * se premuto F6 lancio il pgm.
009800020423if  2c                   if        *INKF
009900020423     c                   exsr      SBMJOB
010000020423     c                   leave
010100020423x   2c                   else
010200020423      * ...altrimenti riemetto la videata
010300020423     c                   iter
010400020423e   2c                   endif
010500020423      *
010600020423e   1c                   enddo
010700020423      *
010800020620     c     Fine          tag
010900020423     c                   eval      *INLR = *on
011000020423
011100020423      ****************************************************************
011200020423      * CONTROLLI VIDEO 1
011300020423      ****************************************************************
011400020423     c     CtrD01        BEGSR
011500020423      *
011600020423sel 1c                   select
011700020423      * controllo data immessa se richiesta ristampa
011800020423wh  1c                   when          V1Cris =  'SI'
011900020423     c                             and V1Cdat <> *zeros
012000020423     c                   clear                   WLBDAT
012100020423     c                   eval      G02dat = V1Cdat
012200020423     c                   call      'XSRDA8'
012300020423     c                   parm                    WLBDAT
012400020423if  2c                   if        G02err = '1'
012500020423     c                   seton                                        28  51
012600020423x   2c                   else
012700020423     c                   eval      V1Cdat = G02dat
012800020423e   2c                   endif
012900020423      * data richiesta se richiesta ristampa
013000020423wh  1c                   when          V1Cris =  'SI'
013100020423     c                             and V1Cdat =  *zeros
013200020423     c                   seton                                        28  52
013300020423      * data non richiesta se non richiesta ristampa
013400020423wh  1c                   when          V1Cris <> 'SI'
013500020423     c                             and V1Cdat <> *zeros
013600020423     c                   seton                                        28  53
013700020423      *
013800020423e   1c                   endsl
013900020423      *
014000020423     c     EctrD01       ENDSR
014100020423
014200020423      ****************************************************************
014300020423      * SOTTOMISSIONE LAVORO
014400020423      ****************************************************************
014500020423     c     SBMJOB        BEGSR
014600020423      *
014700110728     c                   clear                   FIDN76DS
014800110728     c                   movel     V1Cris        I76ris
014900020423if  1c                   if        V1Cris = 'SI'
015000110728     c                   z-add     G02inv        I76dat
015100020423e   1c                   endif
015200120423     c                   movel     V1Cele        I76ele
015300110728     c                   movel(p)  FIDN76DS      KPJBU
015400020423      *
015500110728     c                   movel     'DN77'        KCOAZ
015600110801     c                   movel(p)  'BCH10'       Wpgm             10
015700110801     c****               movel(p)  'FIDN77C'     Wpgm             10
015800050711     c                   call      Wpgm
015900020423     c                   parm                    KPJBA
016000020423      *
016100020423     c                   ENDSR
016200020423
016300020423      ****************************************************************
016400020423      * ROUTINE INIZIALE
016500020423      ****************************************************************
016600020423     c     *INZSR        BEGSR
016700020423      *
016800020423     c     *ENTRY        PLIST
016900020423     c                   PARM                    KPJBA
017000020423      *
017100050711      * Reperisco nome azienda per stampa PREPAGATI
017200050711     c     *dtaara       define    §azute        AZUTEds
017300050711     c     *dtaara       define    §datiute      dDATIUTE
017400050711     c                   in(E)     *dtaara
017500050711     c                   if        %ERROR or RSUT = *blanks
017600050711     c                   clear                   Tibs34Ds
017700050711     c                   call      'TIBS34R'
017800050711     c                   parm                    Tibs34Ds
017900050711     c                   in        *dtaara
018000050711     c                   endif
018100020423      *
018200020423     c                   ENDSR
018300120423      /free
018400120423       //--------------------------------------------------------------
018500120423       //?Esecuzione del comando (già impostato).                      ?
018600120423       //--------------------------------------------------------------
018700120423       BEGSR  sr_ExecCmd;
018800120423
018900120423         clear Qcap0100;
019000120423         Qcabcsdh = *off;
019100120423         Qcapa    = *off;
019200120423         Qcacmdss = *off;
019300120423         Qcaerved = *allX'00';
019400120423
019500120423         clear Qusec;
019600120423         Qusbprv  = %size(Qusec);
019700120423
019800120423         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
019900120423                           %size(Qcap0100) : 'CPOP0100' : *omit :
020000120423                           0 : 0 : Qusec);
020100120423
020200120423         //if  Qusei <> *blank;
020300120423         //  ...;
020400120423         //endif;
020500120423
020600120423       ENDSR;
020700120423      /end-free
