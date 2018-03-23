000100170620     H*--------------------------------------------------------------------------------------------*
000200170620     H* DPD GeoRDB 2017 - Validazione Pattern CAP
000300170620     H*--------------------------------------------------------------------------------------------*
000400170620     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000500170620     H DECEDIT('0,') DATEDIT(*DMY.)
000600170620     F*------------------
000700170620     F* DATA BASE
000800170620     F*------------------
000900170620
001000170620     D*------------------
001100170620     D* DS ESTERNE
001200170620     D*------------------
001300170620     D TISID2DS      e ds
001400170622     D DPCVE10F      e ds
001500170629     D DPCCC10F      e ds
001600170620
001700170620     D*------------------
001800170620     D* VARIABILI D WRK
001900170620     D*------------------
002000170620     D stringaSQL      s           1024A   varying inz
002100170622     D api             s              1A   inz('''')
002200170622     D currCHAR        s              1A   inz
002300170630     D chkCHAR         s              1A   inz
002400170630     D lenPATT         s              1S 0 inz
002500170622     D wSID2PTC        s                   like(ISID2PTC) inz
002600170629     D DS_CCC          ds                  qualified inz
002700170629     D  OSID2PSYS                          like(OSID2PSYS)
002800170629     D  OSID2PVAL                          like(OSID2PVAL)
002900170629     D  OSID2CVAL                          like(OSID2CVAL)
003000170629     D  OSID2DUMY                          like(OSID2DUMY)
003100170629     D  ListaPATTERN                128A   varying
003200170620
003300170620     D*------------------
003400170620     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
003500170620     D*------------------
003600170620     D InStringa       S         131072A   VARYING                              (stringa input)
003700170620     D InSepara        S             10A                                        (separatore)
003800170620     D InTypeOut       S              2A                                        (tipo output)
003900170620     D wSkParam        S         131072A                                        (output)
004000170620     D OutIdxMax       S              4S 0                                      (max indice valoriz)
004100170620     D OutErrore       S              1A                                        (flag errore)
004200170620     D OutMsg          S             80A                                        (messaggio errore)
004300170620     D SkSplitCSV_4    S            128    DIM(512)
004400170620
004500170620     D*-------------------
004600170620     D* COSTANTI
004700170620     D*-------------------
004800170620     D NUM             c                   const('1234567890')                  *numeri
004900170620     D MAIU            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005000170620     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005100170620
005200170620
005300170620     C*--------------------------------------------------------------------------------------------*
005400170620     C* Main lines
005500170620     C*--------------------------------------------------------------------------------------------*
005600150518     C*
005700150518     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
005800150518     C
005900150518     C/EXEC SQL
006000150518     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006100150518     C/END-EXEC
006200170620     C*
006300170620     C* Avvio il monitoring del intero flusso
006400170620     C                   monitor
006500170620     C*
006600170620     C* Inizializzazioni
006700170620     C                   clear                   OSID2VER
006800170622     C                   clear                   OSID2PTC
006900170629     C                   clear                   OSID2PSYS
007000170629     C                   clear                   OSID2PVAL
007100170629     C                   clear                   OSID2CVAL
007200170629     C                   clear                   OSID2DUMY
007300170620     C                   clear                   OSID2ESI
007400170620     C*
007500170620     C* Verifica parametri di input
007600170620     C                   exsr      CHKPAR
007700060516     C*
007800170620     C* Se parametri di input OK => procedo con la verifica del Pattern CAP
007900170620     C   10              exsr      CHKPATT
008000170620     C*
008100170620     C* Se Pattter CAP risultano validati => ritorno parameyri di output
008200170629     C                   eval      OSID2VER  = ISID2VER
008300170629     C                   eval      OSID2PTC  = wSID2PTC
008400170629     C                   eval      OSID2PSYS = DS_CCC.OSID2PSYS
008500170629     C                   eval      OSID2PVAL = DS_CCC.OSID2PVAL
008600170629     C                   eval      OSID2CVAL = DS_CCC.OSID2CVAL
008700170629     C                   eval      OSID2DUMY = DS_CCC.OSID2DUMY
008800170622     C   21              eval      OSID2ESI = 'V'
008900170620     C*
009000170620     C* Gestisco eventuale errore
009100170620     C                   on-error
009200170620     C                   dump(A)
009300170620     C                   eval      OSID2ESI = 'E'
009400170620     C*
009500170620     C* Arresto il monitoring
009600170620     C                   endmon
009700170620     C*
009800170620     C                   seton                                        lr
009900060515     C*
010000170620
010100170620
010200170620     C*------------------------------------------------------------------------*
010300170620     C* CHKPAR - Verifica parametri di input
010400170620     C*------------------------------------------------------------------------*
010500170620     C     CHKPAR        BEGSR
010600170620     C*
010700170620     C                   seton                                        10
010800170620     C*
010900170620     C* Verifica se pasata in input versione o data di riferimento
011000170620     C                   select
011100170620     C*
011200170620     C                   when      ISID2VER > *zeros
011300170620     C*
011400170620     C                   when      ISID2DRI > *zeros
011500170620     C                   exsr      RTVVER
011600170620     C*
011700170620     C                   when      ISID2DRI = *zeros
011800170620     C                   eval      ISID2DRI = datcor
011900170620     C                   exsr      RTVVER
012000170620     C*
012100170620     C                   other
012200170620     C                   eval      OSID2ESI = 'E'
012300170620     C                   setoff                                       10
012400170620     C                   endsl
012500170620     C*
012600170620     C* Verifica che sia specificata la nazione in almeno uno dei 3 formati disponibili
012700170620     C                   if        ISID2NBRT  <> *blanks OR
012800170620     C                             ISID2NISO2 <> *blanks OR
012900170620     C                             ISID2NISON  > *zeros
013000170620     C                   else
013100170620     C                   eval      OSID2ESI = 'E'
013200170620     C                   setoff                                       10
013300170620     C                   endif
013400170620     C*
013500170620     C* Verifica che sia indicato il POSTAL CODE da validare
013600170620     C                   if        ISID2PTC = *blanks
013700170620     C                   eval      OSID2ESI = 'E'
013800170620     C                   setoff                                       10
013900170620     C                   else
014000170620     C*
014100170620     C* Converto in maiuscolo
014200170622     C     minu:MAIU     xlate     ISID2PTC      wSID2PTC
014300170622     C*
014400170622     C* Elido gli spazi interni
014500170622     C                   eval      wSID2PTC = %scanrpl(' ':'':wSID2PTC)
014600170620     C                   endif
014700170620     C*
014800170620     C                   ENDSR
014900170620
015000170620
015100060515     C*------------------------------------------------------------------------*
015200170620     C* RTVVER - Reperimento versione alla data di riferimento indicata in input
015300060515     C*------------------------------------------------------------------------*
015400170620     C     RTVVER        BEGSR
015500170620     C*
015600170620     C* Chiamata al driver preposto per il reperimento versione valida ad una data riferimento
015700170620     C                   call      'TISID1R'
015800170620     C                   parm                    ISID2DRI
015900170622     C                   parm                    ISID2VER
016000170622     C                   parm                    CVEVERD
016100170627     C                   parm                    CVEDDE
016200170627     C                   parm                    CVEDSC
016300170622     C                   parm                    CVEBCID
016400170620     C*
016500170620     C* Se reperita versione valida
016600170620     C                   if        ISID2VER > *zeros
016700170620     C                   else
016800170620     C                   setoff                                       10
016900170620     C                   endif
017000060515     C*
017100060515     C                   ENDSR
017200170620
017300170620
017400170620     C*------------------------------------------------------------------------*
017500170620     C* CHKPATT - Verifica del Pattern CAP
017600170620     C*------------------------------------------------------------------------*
017700170620     C     CHKPATT       BEGSR
017800170622     C*
017900170622     C                   setoff                                       21
018000170629     C                   clear                   DS_CCC
018100170620     C*
018200170629     C                   eval      stringaSQL = 'select CCCPSYS, CCCPVAL,' +
018300170629     C                              ' CCCCVAL, CCCDUMY, CCCPATTL' +
018400170629     C                              ' from DPCCC10F'+
018500170622     C                              ' where CCCVER = '+%editc(ISID2VER:'X')
018600170620     C*
018700170620     C* A seconda del formato codice nazione indicato in input
018800170620     C                   select
018900170620     C*
019000170620     C                   when      ISID2NBRT  <> *blanks
019100170620     C                   eval      stringaSQL = stringaSQL +
019200170622     C                              ' and CCCBRT = '+api+%trim(ISID2NBRT)
019300170620     C*
019400170620     C                   when      ISID2NISO2 <> *blanks
019500170620     C                   eval      stringaSQL = stringaSQL +
019600170622     C                              ' and CCCISO2 = '+api+%trim(ISID2NISO2)
019700170620     C*
019800170620     C                   when      ISID2NISON  > *zeros
019900170620     C                   eval      stringaSQL = stringaSQL +
020000170622     C                              ' and CCCISON = '+api+%editc(ISID2NISON:'4')
020100170620     C*
020200170620     C                   endsl
020300170620     C*
020400170620     C* Considero sempre solamente i record NON annullati
020500170622     C                   eval      stringaSQL = stringaSQL+api+
020600170622     C                                ' and CCCATB = '+api+' '+api
020700170620     C
020800170620     C*
020900170620     C/EXEC SQL
021000170620     C+ PREPARE S1 FROM :stringaSQL
021100170620     C/END-EXEC
021200170620     C
021300170620     C/EXEC SQL
021400170620     C+ DECLARE C1 CURSOR FOR S1
021500170620     C/END-EXEC
021600170620     c*
021700170620     C/EXEC SQL
021800170620     C+ OPEN C1
021900170620     C/END-EXEC
022000170620     C
022100170620     C/EXEC SQL
022200170629     C+ FETCH NEXT FROM C1 INTO :DS_CCC
022300170620     C/END-EXEC
022400170620     C
022500170620     C*
022600170620     C* Si assume che vi sia un unica NAZIONE reperita
022700170620     C                   if        sqlcod = *zeros
022800170620     C*
022900170620     C* Splitto il campo "lista" ed effettuo la verifica rispetto a ciascun PATTERN
023000170629     C                   eval      InStringa = DS_CCC.ListaPATTERN
023100170620     C                   eval      InSepara = ','
023200170620     C                   exsr      SR_SPLIT
023300170620     C*
023400170622     C* Sviluppo tutti gli elementi della lista dei POSTAL CODE PATTERNs
023500170620     C                   z-add     1             j                 4 0
023600170630     C                   dow       j <= %elem(SkSplitCSV_4) AND
023700170620     C                             j <= OutIdxMax
023800170622     C*
023900170622     C* Inizializzo validità CAP a OK
024000170622     C                   seton                                        20
024100170620     C*
024200170620     C* Effettuo il parsing di ogni elemento
024300170620     C                   z-add     1             i                 3 0
024400170630     C                   eval      lenPATT = %len(%trim(SkSplitCSV_4(j)))
024500170630     C                   dow       i <= lenPATT AND *in20
024600170620     C*
024700170622     C                   eval      currCHAR = %subst(wSID2PTC:i:1)
024800170630     C                   eval      chkCHAR  = %subst(SkSplitCSV_4(j):i:1)
024900170622     C*
025000170620     C                   select
025100170620     C*
025200170630     C                   when      chkCHAR = 'A'
025300170622     C                   if        %scan(currCHAR:MAIU) > *zeros
025400170620     C                   else
025500170620     C                   setoff                                       20
025600170620     C                   endif
025700170620     C*
025800170630     C                   when      chkCHAR = 'B'
025900170622     C                   if        %scan(currCHAR:MAIU) > *zeros OR
026000170622     C                                   currCHAR      = *blanks
026100170620     C                   else
026200170620     C                   setoff                                       20
026300170620     C                   endif
026400170620     C*
026500170630     C                   when      chkCHAR = 'N'
026600170622     C                   if        %scan(currCHAR:NUM) > *zeros
026700170620     C                   else
026800170620     C                   setoff                                       20
026900170620     C                   endif
027000170620     C
027100170620     C*
027200170630     C                   when      chkCHAR = 'O'
027300170622     C                   if        %scan(currCHAR:NUM) > *zeros OR
027400170622     C                                   currCHAR      = *blanks
027500170620     C                   else
027600170620     C                   setoff                                       20
027700170620     C                   endif
027800170620     C*
027900170630     C                   when      chkCHAR = '?'
028000170622     C                   if        %scan(currCHAR:MAIU) > *zeros OR
028100170622     C                             %scan(currCHAR:NUM)  > *zeros
028200170620     C                   else
028300170620     C                   setoff                                       20
028400170620     C                   endif
028500170620     C*
028600170630     C                   when      chkCHAR = 'X'
028700170622     C                   if        %scan(currCHAR:MAIU) > *zeros OR
028800170622     C                             %scan(currCHAR:NUM)  > *zeros OR
028900170622     C                                   currCHAR       = *blanks
029000170620     C                   else
029100170620     C                   setoff                                       20
029200170620     C                   endif
029300170620     C*
029400170620     C                   endsl
029500170620     C*
029600170620     C                   add       1             i
029700170620     C                   enddo
029800170622     C*
029900170622     C                   if        *in20
030000170622     C                   eval      *in21 = *on
030100170622     C                   leave
030200170622     C                   endif
030300170620     C*
030400170620     C                   add       1             j
030500170620     C                   enddo
030600170620     C*
030700170620     C                   endif
030800170620     C*
030900170620     C
031000170620     C/EXEC SQL
031100170620     C+ CLOSE C1
031200170620     C/END-EXEC
031300170620     C
031400170620     C*
031500170620     C                   ENDSR
031600170620
031700170620
031800170620     C*----------------------------------------------------*
031900170620     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
032000170620     C*----------------------------------------------------*
032100170620     C     SR_SPLIT      BEGSR
032200170620     C*
032300170620     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
032400170620     C                   EVAL      InTypeOut = '4'
032500170620     C                   CALL      'XSPLIT3'
032600170620     C                   PARM                    InStringa
032700170620     C                   PARM                    InSepara
032800170620     C                   PARM                    InTypeOut
032900170620     C                   PARM      ''            wSkParam
033000170622     C                   PARM                    OutIdxMax
033100170620     C                   PARM                    OutErrore
033200170620     C                   PARM                    OutMsg
033300170620     C                   MOVEA     wSkParam      SkSplitCSV_4
033400170620     C*
033500170620     C                   ENDSR
033600170620
033700170620
033800170620     C*--------------------------------------------------------------------------------------------*
033900060515     C* *inzsr - operazioni iniziali
034000060515     C*--------------------------------------------------------------------------------------------*
034100000000     C     *inzsr        BEGSR
034200060515     C*
034300060515     C* Ricevimento parametri
034400060515     C     *ENTRY        PLIST
034500170620     C                   PARM                    TISID2DS
034600060510     C*
034700060510     C* CALCOLA LA DATA CORRENTE
034800170620     C                   Z-ADD     *zeros        datcor            8 0
034900170620     C                   EVAL      datcor = %dec(%date() : *iso)
035000060515     C*
035100060515     C                   ENDSR
