000100120424     /*PRM  dbgview(*source)
000200120424     /*END
000300021121      *--------------------------------------------------------------*
000400021121      *    Elenco C.A. per rivalsa FEDEX - filtro di lancio          *
000500021121      *--------------------------------------------------------------*
000600021121      *
000700021121      ****************************************************************
000800021121      *  RIEPILOGO INDICATORI
000900021121      ****************************************************************
001000021121      * 28 - ERRORE GENERICO DSPF
001100021121      * 50 - ERRORE Data errata
001200021121      * 51 - ERRORE Data obbligatoria in caso di RISTAMPA
001300021121      * 52 - ERRORE Data non richiesta in caso di 1a STAMPA
001400021121      * 90 - ERRORE GENERICO
001500021121      ***************************************************************
001600021121
001700021121     H DECEDIT('0,') DATEDIT(*DMY.)
001800021121
001900021121     FFIDN73D   CF   E             WORKSTN
002000021121
002100021121     D KPJBA         e ds
002200021121     D FIDN73DS      e ds                  inz
002300021121      *
002400050711      * - Parametri x Controllo profilo utenti
002500050711     d TIBS34ds      e ds
002600050711      * - Ds di riferimento al file esterno AZUTE00F
002700050711     d AZUTEds       e ds                  extname(AZUTE00F)
002800050711      * - Ds per dati organigramma
002900050711     d dDATIUTE      e ds
003000021121      *
003100021121     D STATUS         sds
003200021121     D  VTCPGM           *proc
003300021121      *
003400021121     D WLBDAT          ds                  inz
003500021121     D  G02DAT                 1      8  0
003600021121     D  G02INV                 9     16  0
003700021121     D  G02ERR                17     17
003800021121     D  G02TGI                18     22  0
003900021121      *
004000021121     D $FINE           s              1    inz(*off)
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
005400021121
005500021121      *--------------------------------------------------------------*
005600021121      *                   C   I   C   L   O                          *
005700021121      *--------------------------------------------------------------*
005800021121
005900120424     c                   eval      V1Cele = 'F'
006000120424     c                   if        %subst(knsif:7:1)='P'
006100120424     c                   eval      V1Cfile = 'GAITRAAZP/WFDPD00F'
006200120424     c                   else
006300120424     c                   eval      V1Cfile = 'GAITRAAZM/WFDPD00F'
006400120424     c                   endif
006500120424      *
006600021121do  1c                   do        *hival
006700021121      *
006800021121      * Emissione VIDEO1
006900120424     c                   write     FI73T01
007000120424     c                   write     FI73Z01
007100021121     c                   exfmt     FI73D01
007200021121     c                   setoff                                       2890
007300120424sel 2c                   select
007400021121      * F3=Fine
007500120424w   2c                   when      *INKC
007600021121     c                   leave
007700120424      * F9=Visualizzazione WFDPD00F
007800120424if  2c                   when      *INKI
007900120424     c                   eval      Qcmd = 'DSPDBF file(*libl/WFDFE00F) +
008000120424     c                                            mbr(*first) output(*)'
008100120424     c                   exsr      sr_ExecCmd
008200120424     c                   iter
008300120424e   2c                   endsl
008400021121      * Controlli videata
008500021121     c                   exsr      CtrD01
008600021121      * se è acceso il 28 ritorno ad emettere il formato video
008700021121if  2c                   if        *in28 or *in90
008800021121     c                   iter
008900021121e   2c                   endif
009000021121      *
009100021121      * se premuto F6 lancio il pgm.
009200021121if  2c                   if        *INKF
009300021121     c                   exsr      SBMJOB
009400021121     c                   leave
009500021121x   2c                   else
009600021121      * ...altrimenti riemetto la videata
009700021121     c                   iter
009800021121e   2c                   endif
009900021121      *
010000021121e   1c                   enddo
010100021121      *
010200021121     c     Fine          tag
010300021121     c                   eval      *INLR = *on
010400021121
010500021121      ****************************************************************
010600021121      * CONTROLLI VIDEO 1
010700021121      ****************************************************************
010800021121     c     CtrD01        BEGSR
010900021121      *
011000021121sel 1c                   select
011100021121      * controllo data immessa se richiesta ristampa
011200021121wh  1c                   when          V1Cris =  'SI'
011300021121     c                             and V1Cdat <> *zeros
011400021121     c                   clear                   WLBDAT
011500021121     c                   eval      G02dat = V1Cdat
011600021121     c                   call      'XSRDA8'
011700021121     c                   parm                    WLBDAT
011800021121if  2c                   if        G02err = '1'
011900021121     c                   seton                                        28  51
012000021121x   2c                   else
012100021121     c                   eval      V1Cdat = G02dat
012200021121e   2c                   endif
012300021121      * data richiesta se richiesta ristampa
012400021121wh  1c                   when          V1Cris =  'SI'
012500021121     c                             and V1Cdat =  *zeros
012600021121     c                   seton                                        28  52
012700021121      * data non richiesta se non richiesta ristampa
012800021121wh  1c                   when          V1Cris <> 'SI'
012900021121     c                             and V1Cdat <> *zeros
013000021121     c                   seton                                        28  53
013100021121      *
013200021121e   1c                   endsl
013300021121      *
013400021121     c     EctrD01       ENDSR
013500021121
013600021121      ****************************************************************
013700021121      * SOTTOMISSIONE LAVORO
013800021121      ****************************************************************
013900021121     c     SBMJOB        BEGSR
014000021121      *
014100021121     c                   clear                   FIDN73DS
014200021121     c                   movel     V1Cris        I73ris
014300021121if  1c                   if        V1Cris = 'SI'
014400021121     c                   z-add     G02inv        I73dat
014500021121e   1c                   endif
014600120424     c                   movel     V1Cele        I73ele
014700021121     c                   movel(p)  FIDN73DS      KPJBU
014800021121      *
014900021121     c                   movel     'DN74'        KCOAZ
015000050711     ***c                   call      'FIDN74R'
015100050711     ***c                   call      'BCH10'
015200050711     c                   movel(p)  'BCH10'       Wpgm             10
015300050711     c                   call      Wpgm
015400021121     c                   parm                    KPJBA
015500021121      *
015600021121     c                   ENDSR
015700021121
015800021121      ****************************************************************
015900021121      * ROUTINE INIZIALE
016000021121      ****************************************************************
016100021121     c     *INZSR        BEGSR
016200021121      *
016300021121     c     *ENTRY        PLIST
016400021121     c                   PARM                    KPJBA
016500021121      *
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
017600021121      *
017700021121     c                   ENDSR
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
