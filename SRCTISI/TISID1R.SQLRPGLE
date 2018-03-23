000100170620     H*--------------------------------------------------------------------------------------------*
000200170620     H* DPD GeoRDB 2017 - Reperimento Versione
000300170620     H*--------------------------------------------------------------------------------------------*
000400170620     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000500170620     H DECEDIT('0,') DATEDIT(*DMY.)
000600170620     F*------------------
000700170620     F* DATA BASE
000800170620     F*------------------
000900170620     D*------------------
001000170620     D* DS ESTERNE
001100170622     D*------------------
001200170620     D*------------------
001300170620     D* VARIABILI D WRK
001400170620     D*------------------
001500170912     D/COPY GAITRASRC/SRCPROTOPI,TISID1R
001600170622     D wVERSIONE       s                   like(OSID1VER)  inz
001700170622     D wVERS_DPD       s                   like(OSID1VERD) inz
001800170627     D wDATA_DEC       s                   like(OSID1DDE)  inz
001900170627     D wDATA_SCA       s                   like(OSID1DSC)  inz
002000170622     D wBARCODEID      s                   like(OSID1BCID) inz
002100170620     C*--------------------------------------------------------------------------------------------*
002200170620     C* Main lines
002300170620     C*--------------------------------------------------------------------------------------------*
002400150518     C*
002500150518     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
002600150518     C
002700150518     C/EXEC SQL
002800150518     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
002900150518     C/END-EXEC
003000170620     C*
003100170620     C* Avvio il monitoring del intero flusso
003200170620     C                   monitor
003300170620     C*
003400170620     C* Inizializzazioni
003500170620     C                   clear                   wVERSIONE
003600170622     C                   clear                   wVERS_DPD
003700170627     C                   clear                   wDATA_DEC
003800170627     C                   clear                   wDATA_SCA
003900170622     C                   clear                   wBARCODEID
004000170620     C                   clear                   OSID1VER
004100170622     C                   clear                   OSID1VERD
004200170627     C                   clear                   OSID1DDE
004300170627     C                   clear                   OSID1DSC
004400170622     C                   clear                   OSID1BCID
004500060516     C*
004600170620     C* Se data di riferimento in input non specificata assumo la data corrente
004700170620     C                   if        ISID1DRI = *zeros                            * solo chiusura
004800170620     C                   eval      ISID1DRI = datcor
004900060516     C                   endif
005000060516     C*
005100170620     C* Reperisco la versione valida alla data
005200170620     C                   exsr      RTVVER
005300170620     C*
005400170620     C* Gestisco eventuale errore
005500170620     C                   on-error
005600170620     C                   dump(A)
005700170620     C                   eval      OSID1VER = -3
005800170620     C*
005900170620     C* Arresto il monitoring
006000170620     C                   endmon
006100170620     C*
006200170620     C                   seton                                        lr
006300060515     C*
006400170620
006500170620
006600060515     C*------------------------------------------------------------------------*
006700170620     C* RTVVER - Reperimento versione alla data di riferimento indicata in input
006800060515     C*------------------------------------------------------------------------*
006900170620     C     RTVVER        BEGSR
007000170620     C*
007100170620     C/EXEC SQL
007200170620     C+ declare C1 cursor for
007300170627     C+ select CVEVER, CVEVERD,
007400170627     C+        CVEDDE, CVEDSC, CVEBCID from DPCVE10F
007500170620     C+ where :ISID1DRI between CVEDDE and CVEDSC and CVEATB = ' '
007600170620     C+ for read only
007700170620     C/END-EXEC
007800170620     C
007900170620     C/EXEC SQL
008000170620     C+ open C1
008100170620     C/END-EXEC
008200170620     C*
008300170620     C* Verifico se trovata 1 (SOLA) versione valida
008400170620     C                   z-add     *zeros        i                 1 0
008500170620     C                   dow       sqlcod = *zeros AND i <= 1
008600170620     C
008700170620     C/EXEC SQL
008800170627     C+ Fetch next from C1 into :wVERSIONE, :wVERS_DPD,
008900170627     C+                         :wDATA_DEC, :wDATA_SCA, :wBARCODEID
009000170627     C/END-EXEC
009100170620     C
009200170622     C                   if        sqlcod <> 100
009300170620     C                   add       1             i
009400170622     C                   endif
009500170620     C                   enddo
009600170620     C*
009700170620     C* Verifico esiti ricerca
009800170620     C                   select
009900170620     C*
010000170620     C* Se NON trovata versione => errore
010100170620     C                   when      i = *zeros
010200170620     C                   eval      wVERSIONE = -1
010300170620     C*
010400170620     C* Se trovata più di una versione => errore + alert
010500170620     C                   when      i > 1
010600170620     C                   eval      wVERSIONE = -2
010700170620     C                   exsr      sndAlert
010800170620     C*
010900170620     C* Se trovata 1 SOLA versione => OK
011000170622     C                   when      i = 1 AND wVERSIONE > *zeros
011100170622     C                   eval      OSID1VER  = wVERSIONE
011200170622     C                   eval      OSID1VERD = wVERS_DPD
011300170627     C                   eval      OSID1DDE  = wDATA_DEC
011400170627     C                   eval      OSID1DSC  = wDATA_SCA
011500170622     C                   eval      OSID1BCID = wBARCODEID
011600170620     C*
011700170620     C                   endsl
011800170620     C
011900170620     C/EXEC SQL
012000170620     C+ close C1
012100170620     C/END-EXEC
012200170620     C
012300060515     C*
012400060515     C                   ENDSR
012500170620
012600170620
012700170620     C*--------------------------------------------------------------------------------------------*
012800170620     C* SNDALERT - Invio email di alert al assistenza IT
012900170620     C*--------------------------------------------------------------------------------------------*
013000170620     C     sndAlert      BEGSR
013100170620     C*
013200170620     C* Inizializzo variabili
013300170620     C                   movel     *blanks       wrkEml          253
013400170620     C                   movel     *blanks       wrkCcEml        253
013500170620     C                   movel     *blanks       wrkEmlMsg      5000
013600170620     C                   movel     *blanks       wrkEmlOgg        44
013700170620     C*
013800170620     C* Valorizzo i campi della e-m@ail
013900170620     C                   eval      wrkEml='cedalert@brt.it'
014000171025     C                   eval      wrkCcEml='andrea.bertocchi@brt.it'
014100170620     C                   eval      wrkEmlOgg=
014200170620     C                               'DPD GeoRDB 2017 - Reperimento Versione'
014300170620     C                   EVAL      wrkEmlMsg = 'Per data riferimento:  '       +
014400170620     C                                         %editc(ISID1DRI:'X')            +
014500170620     C                                                                    ':/N'+
014600170620     C                                                                    ':/N'+
014700170620     C                             'trovata più di una versione, VERIFICARE'   +
014800170620     C                                                                    ':/N'
014900170620     C*
015000170620     C                   call(e)   'TIS701C1'
015100170620     C                   parm                    wrkEml
015200170620     C                   parm                    wrkCcEml
015300170620     C                   parm                    wrkEmlOgg
015400170620     C                   parm                    wrkEmlMsg
015500170620     C                   parm                    wrkEsito          1
015600170620     C*
015700170620     C                   ENDSR
015800170620
015900170620
016000170620     C*--------------------------------------------------------------------------------------------*
016100060515     C* *inzsr - operazioni iniziali
016200060515     C*--------------------------------------------------------------------------------------------*
016300000000     C     *inzsr        BEGSR
016400060515     C*
016500060515     C* Ricevimento parametri
016600060515     C     *ENTRY        PLIST
016700170620     C                   PARM                    ISID1DRI
016800170620     C                   PARM                    OSID1VER
016900170622     C                   PARM                    OSID1VERD
017000170627     C                   PARM                    OSID1DDE
017100170627     C                   PARM                    OSID1DSC
017200170622     C                   PARM                    OSID1BCID
017300060510     C*
017400060510     C* CALCOLA LA DATA CORRENTE
017500170620     C                   Z-ADD     *zeros        datcor            8 0
017600170620     C                   EVAL      datcor = %dec(%date() : *iso)
017700060515     C*
017800060515     C                   ENDSR
