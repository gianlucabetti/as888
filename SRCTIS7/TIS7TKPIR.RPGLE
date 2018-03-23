000100170608     H DECEDIT('0,') DATEDIT(*DMY.)
000200120604     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400170608     FTIVGDTMP  IF   E             DISK
000500040719     FTIVAWWWT  UF A E             DISK
000600030924
000700170608     D DS_KPI        e ds                  extname(wfkpi00f)
000800071121
000900071121
001000071121     D*------------
001100071121     D* VARIABILI D WRK
001200071121     D*------------
001300030924     D prmppt          S             50
001400030924     D prmesito        S              1
001500030924     D wrkesito        S                   like(prmesito)
001600170609     D CSV             S              1    inz
001700170609     D I               S              1    inz
001800170609     D F               S              2    inz
001900071122     D wBufferOut      s          10000    varying inz
002000120604     D wDataSped       s             10    inz
002100120604     D wDataCRich      s             10    inz
002200120604     D wDataStato      s             10    inz
002300150707     D wOraStato       s              5    inz
002400071211     D*
002500100910
002600100910
002700100910     D*------------------
002800100910     D* LINKING A DEFINIZIONI ESTERNE
002900100910     D*------------------
003000170608xxx  D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
003100170608xxx  D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
003200100910
003300030924
003400030325
003500030924     C* MAIN/
003600071121     C*
003700071121     C* Verifico se già presenti dati nel file d output
003800071121     C                   EXSR      chkRECFILMBR
003900071121     C*
004000071121     C* Effettuo elaborazione
004100071121     C                   EXSR      traduci
004200071121     C*
004300921023     C                   SETON                                        LR
004400991027
004500071121
004600071121
004700071121     C     chkRECFILMBR  BEGSR
004800071121     C*
004900071121     C                   READ      TIVAWWWT                               55
005000071121     C*
005100071121     C                   ENDSR
005200071121
005300071121
005400030924
005500991027     C     traduci       BEGSR
005600071121     C*
005700071121     C* Inizializzazone formato e variabili x output
005800170608     C                   clear                   DS_KPI
005900071121     C                   clear                   wBufferOut
006000170608     C*
006100170608     C* Se richiesto nei parametri separatore campo specifico adotto, altrimento standard
006200170608     C                   IF        %subst(prmppt:2:1) <> *blanks
006300170609     C                   EVAL      CSV = %subst(prmppt:2:1)
006400170608     C                   ELSE
006500170609     C                   EVAL      CSV = ';'
006600170608     C                   ENDIF
006700170609     C*
006800170609     C* Imposto i caratteri di Inizio/Fine singola colonna
006900170609     C                   EVAL      I = '"'
007000170609     C                   EVAL      F = '"' + CSV
007100071121     C*
007200071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
007300071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
007400100908     C                   EVAL      wBufferOut =
007500170609     C***                            +'FIL.BRT'                               +F
007600170609     C***                          +I+'USO INTERNO BRT'                       +F
007700170609     C***                          +I+'COD.CLIENTE MITTENTE SE ASSEGNATO'     +F
007800170609     C                              I+'COD.CLIENTE MITTENTE SE ASSEGNATO'     +F
007900170609     C***                          +I+'RAG.SOCIALE MITTENTE 1'                +F
008000170609     C                             +I+'ANNO SPEDIZIONE'                       +F
008100170609     C                             +I+'MESE RIFERIMENTO BOLLE'                +F
008200170609     C                             +I+'RIFERIMENTO MITTENTE NUM.'             +F
008300170609     C                             +I+'RIFERIM.MITTENTE ALFANUMER'            +F
008400170609     C                             +I+'FILIALE PARTENZA'                      +F
008500170609     C                             +I+'NUMERO SERIE'                          +F
008600170609     C                             +I+'NUMERO SPEDIZIONE'                     +F
008700170609     C                             +I+'MESE/GIORNO SPEDIZIONE'                +F
008800170609     C                             +I+'FILIALE ARRIVO'                        +F
008900170609     C                             +I+'RAGIONE SOCIALE DESTINATARIO'          +F
009000170609     C                             +I+'LOCALITA DESTINATARIO'                 +F
009100170609     C                             +I+'PROVINCIA DESTINATARIO'                +F
009200170609     C                             +I+'CAP DESTINATARIO'                      +F
009300170609     C                             +I+'NAZIONE DESTINATARIO'                  +F
009400170609     C                             +I+'FLAG FERMO DEPOSITO: S=SI'             +F
009500170609     C                             +I+'1 CONSEGNA PARTICOLARE'                +F
009600170609     C                             +I+'2 CONSEGNA PARTICOLARE'                +F
009700170609     C                             +I+'1° GG.CHIUSURA DESTINATARIO'           +F
009800170609     C                             +I+'2° GG.CHIUSURA DESTINATARIO'           +F
009900170609     C                             +I+'NUMERO COLLI'                          +F
010000170609     C                             +I+'PESO KG BOLLETTATO'                    +F
010100170609     C                             +I+'VOLUME BOLLETTATO'                     +F
010200170609     C                             +I+'TIPO DT.CONSEGNA:IL/DOPO/PRIMA'        +F
010300170609     C                             +I+'DATA CONSEGNA RICHIESTA'               +F
010400170609     C                             +I+'DATA 1A CONSEGNA PARZIALE'             +F
010500170609     C                             +I+'DATA CONSEGNA MERCE (REALE)'           +F
010600170609     C                             +I+'ORA CONSEGNA MERCE (REALE)'            +F
010700170609     C                             +I+'CODICE CONSEGNA ANOMALA'               +F
010800170609     C                             +I+'DESCRIZIONE CONS.ANOM'                 +F
010900170609     C                             +I+'ORE TEORICHE DI DELIVERY'              +F
011000170609     C                             +I+'ORE REALI    DI DELIVERY'              +F
011100170609     C                             +I+'ORE NON CONFORMITA'                    +F
011200170609     C                             +I+'COLLI PARTITI IN 88'                   +F
011300170609     C                             +I+'DATA SPUNTA ENTRATA COLLO 88'          +F
011400170609     C                             +I+'S=HA AVUTO IMP/DISGUIDO'               +F
011500170609     C                             +I+'CAUSALE RITARDO TRAZIONE'              +F
011600170609     C                             +I+'MOTIVO RITARDO TRAZIONE'               +F
011700170609     C                             +I+'DATA ARRIVO MERCE A FIL. ARRIVO'       +F
011800170609     C                             +I+'DATA GIACENZA: APERTURA'               +F
011900170609     C                             +I+'DATA GIACENZA: CHIUSURA'               +F
012000170609     C                             +I+'DATA EVENTO 1'                         +F
012100170609     C                             +I+'COD. EVENTO 1'                         +F
012200170609     C                             +I+'DATA EVENTO 2'                         +F
012300170609     C                             +I+'COD. EVENTO 2'                         +F
012400170609     C                             +I+'DATA EVENTO 3'                         +F
012500170609     C                             +I+'COD. EVENTO 3'                         +F
012600170609     C                             +I+'DATA EVENTO 4'                         +F
012700170609     C                             +I+'COD. EVENTO 4'                         +F
012800170609     C                             +I+'DATA EVENTO 5'                         +F
012900170609     C                             +I+'COD. EVENTO 5'                         +F
013000170609     C                             +I+'DATA EVENTO 6'                         +F
013100170609     C                             +I+'COD. EVENTO 6'                         +F
013200170609     C                             +I+'DATA EVENTO 7'                         +F
013300170609     C                             +I+'COD. EVENTO 7'                         +F
013400170609     C                             +I+'DATA EVENTO 8'                         +F
013500170609     C                             +I+'COD. EVENTO 8'                         +F
013600170609     C                             +I+'DATA EVENTO 9'                         +F
013700170609     C                             +I+'COD. EVENTO 9'                         +F
013800170609     C                             +I+'DATA EVENTO 10'                        +F
013900170609     C                             +I+'COD. EVENTO 10'                        +F
014000170609     C                             +I+'DATA C.A.: APERTURA'                   +F
014100170609     C                             +I+'DATA C.A.: CHIUSURA'                   +F
014200170609     C                             +I+'INOLTRO: P=PROV BLANK-CITTA I=ISOLA'   +F
014300170609     C                             +I+'DATA PREVISTA CONSEGNA'                +F
014400170609     C                             +I+'TENTATIVO CONS. IN 1st DATA PREV S/N/P'+F
014500170608     C*
014600071121     C                   EVAL      VAWDTA = wBufferOut
014700071121     C                   WRITE     TIVAW000
014800071121     C                   ENDIF
014900991027     C*
015000080924     C                   READ      TIVGDTMP
015100060119     C*
015200170608     C* Leggo tutto il file
015300080924     C                   DOW       not %eof(TIVGDTMP)
015400080924     C*
015500170608     C* Ridefinisco dati da tradurre
015600170608     C                   eval      DS_KPI = vgdDTA
015700071121     C*
015800170608     C* Inizializzo buffer di output
015900071121     C                   clear                   wBufferOut
016000170608     C                   clear                   TIVAW000
016100071121     C*
016200071121     C* Compondo la stringa dati output relativa al record corrente
016300071121     C                   EXSR      wrifile
016400040719     C*
016500040719     C* Proseguo con la lettura dell'input
016600080924     C                   READ      TIVGDTMP
016700060119     C*
016800030325     C                   ENDDO
016900071121     C*
017000030325     C                   EVAL      wrkesito = '0'
017100991027     C*
017200910830     C                   ENDSR
017300071121
017400071121
017500071121
017600071121     C     wrifile       BEGSR
017700100908     C*
017800100908     C                   EVAL      wBufferOut =
017900170609     C***                           I+%trim(%editc(KPIEPO:'4'))               +F
018000170609     C***                          +I+%trim(KPIIDUTE)                         +F
018100170609     C***                          +I+%trim(%editc(KPICCM:'X'))               +F
018200170609     C                              I+%trim(%editc(KPICCM:'X'))               +F
018300170609     C***                          +I+%trim(KPIRSM)                           +F
018400170609     C                             +I+%trim(%editc(KPIAAS:'4'))               +F
018500170609     C                             +I+%trim(%editc(KPIMES:'4'))               +F
018600170609     C                             +I+%trim(%editc(KPIRMN:'4'))               +F
018700170609     C                             +I+%trim(KPIRMA)                           +F
018800170609     C                             +I+%trim(%editc(KPILNP:'4'))               +F
018900170609     C                             +I+%trim(%editc(KPINRS:'4'))               +F
019000170609     C                             +I+%trim(%editc(KPINSP:'4'))               +F
019100170609     C                             +I+%trim(%editc(KPIMGS:'X'))               +F
019200170609     C                             +I+%trim(%editc(KPILNA:'4'))               +F
019300170609     C                             +I+%trim(KPIRSD)                           +F
019400170609     C                             +I+%trim(KPILOD)                           +F
019500170609     C                             +I+%trim(KPIPRD)                           +F
019600170609     C                             +I+%trim(KPICAD)                           +F
019700170609     C                             +I+%trim(KPINZD)                           +F
019800170609     C                             +I+%trim(KPIFFD)                           +F
019900170609     C                             +I+%trim(KPITC1)                           +F
020000170609     C                             +I+%trim(KPITC2)                           +F
020100170609     C                             +I+%trim(KPIGC1)                           +F
020200170609     C                             +I+%trim(KPIGC2)                           +F
020300170609     C                             +I+%trim(%editc(KPINCL:'4'))               +F
020400170609     C                             +I+%trim(%editc(KPIPKB:'4'))               +F
020500170609     C                             +I+%trim(%editc(KPIVLB:'4'))               +F
020600170609     C                             +I+%trim(KPITCR)                           +F
020700170609     C                             +I+%trim(%editc(KPIDCR:'4'))               +F
020800170609     C                             +I+%trim(%editc(KPIDCP:'4'))               +F
020900170609     C                             +I+%trim(%editc(KPIDCM:'4'))               +F
021000170609     C                             +I+%trim(%editc(KPIHMC:'X'))               +F
021100170609     C                             +I+%trim(KPICCA)                           +F
021200170609     C                             +I+%trim(KPICCA_DES)                       +F
021300170609     C                             +I+%trim(%editc(KPIDLY_T:'4'))             +F
021400170609     C                             +I+%trim(%editc(KPIDLY_R:'4'))             +F
021500170609     C                             +I+%trim(%editc(KPIDLY_C:'4'))             +F
021600170609     C                             +I+%trim(KPIP88)                           +F
021700170609     C                             +I+%trim(%editc(KPIDSE:'4'))               +F
021800170609     C                             +I+%trim(KPIIMPDS)                         +F
021900170609     C                             +I+%trim(KPIR_TRZ)                         +F
022000170609     C                             +I+%trim(KPIR_TRZD)                        +F
022100170609     C                             +I+%trim(%editc(KPIDAM:'4'))               +F
022200170609     C                             +I+%trim(%editc(KPIDAG:'4'))               +F
022300170609     C                             +I+%trim(%editc(KPIDCG:'4'))               +F
022400170609     C                             +I+%trim(%editc(KPIDEV1:'4'))              +F
022500170609     C                             +I+%trim(KPICEV1)                          +F
022600170609     C                             +I+%trim(%editc(KPIDEV2:'4'))              +F
022700170609     C                             +I+%trim(KPICEV2)                          +F
022800170609     C                             +I+%trim(%editc(KPIDEV3:'4'))              +F
022900170609     C                             +I+%trim(KPICEV3)                          +F
023000170609     C                             +I+%trim(%editc(KPIDEV4:'4'))              +F
023100170609     C                             +I+%trim(KPICEV4)                          +F
023200170609     C                             +I+%trim(%editc(KPIDEV5:'4'))              +F
023300170609     C                             +I+%trim(KPICEV5)                          +F
023400170609     C                             +I+%trim(%editc(KPIDEV6:'4'))              +F
023500170609     C                             +I+%trim(KPICEV6)                          +F
023600170609     C                             +I+%trim(%editc(KPIDEV7:'4'))              +F
023700170609     C                             +I+%trim(KPICEV7)                          +F
023800170609     C                             +I+%trim(%editc(KPIDEV8:'4'))              +F
023900170609     C                             +I+%trim(KPICEV8)                          +F
024000170609     C                             +I+%trim(%editc(KPIDEV9:'4'))              +F
024100170609     C                             +I+%trim(KPICEV9)                          +F
024200170609     C                             +I+%trim(%editc(KPIDEV0:'4'))              +F
024300170609     C                             +I+%trim(KPICEV0)                          +F
024400170609     C                             +I+%trim(%editc(KPIDAC:'4'))               +F
024500170609     C                             +I+%trim(%editc(KPIDCC:'4'))               +F
024600170609     C                             +I+%trim(KPIFIN)                           +F
024700170609     C                             +I+%trim(%editc(KPIDCE:'4'))               +F
024800170609     C                             +I+%trim(KPIDCESN)                         +F
024900170609     C*
025000071121     C                   EVAL      VAWDTA = wBufferOut
025100110519     C*
025200071121     C                   WRITE     TIVAW000
025300071121     C*
025400071121     C                   ENDSR
025500071121
025600071121
025700071121
025800991027
025900991027      /TITLE Operazioni iniziali.
026000991027     C     *inzsr        BEGSR
026100991027     C*
026200991027     C     *ENTRY        PLIST
026300991027     C                   parm                    prmppt
026400991027     C     wrkesito      parm      wrkesito      prmesito
026500991027     C*
026600991027     C                   ENDSR
026700071121     C*
