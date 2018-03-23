000100170412     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000200990907
000300170412
000400170412      //********************************************************************************************
000500170412      //
000600170412      // Definizione prototipi procedure.
000700170412      //
000800170412      //********************************************************************************************
000900170412
001000170412
001100170412      //********************************************************************************************
001200170412      //
001300170412      // Definizione interfacce procedure.
001400170412      //
001500170412      //********************************************************************************************
001600170804     D/COPY GAITRASRC/SRCPROTOPI,UBSGU1R
001700170804     D/COPY GAITRASRC/SRCPROTOPI,UBSI95R
001800170804     D/COPY GAITRASRC/SRCPROTOPI,UBAASGNR
001900170803     D/COPY GAITRASRC/SRCPROTOPI,UBS717R
002000170824     D/COPY GAITRASRC/SRCPROTOPI,TIS713R
002100170516
002200170804     D*
002300170804     D  DVPC         e ds                  inz
002400170804     D  UBSGU1DSA    e ds                  inz qualified
002500170804     D  UBBRTETIDS   e ds                  inz qualified
002600170804     D RTNsi95esito    s             10I 0
002700170804     D RTNsi95opcode   s             10A
002800170804     D RTNsi95status   s             10I 0
002900170824     D dftOUTQ         s             10A
003000170824     D dftTIPF_ETI     s              2A
003100170824     D dftTIP_TRASF    s              2A
003200170804
003300010525
003400010525     C*-----------------------------------------------------------------------------
003500170412     C*
003600170412     C* Avvio il monitoring del intero flusso
003700170412     C                   monitor
003800170804     C*
003900170804     C* Inizializzazioni
004000170804     C                   exsr      exeInz
004100170824     C*
004200170824     C* Caricamento tabella
004300170824     C                   exsr      carTab
004400170803     C*
004500170803     C* Eseguo l'opzione richiesta
004600170803     C                   exsr      exeOpz
004700170412     C*
004800170412     C* Gestisco eventuale errore
004900170412     C                   on-error
005000170518     C
005100170803     C                   eval      oUBS717Esito = -1
005200170412     C                   dump(A)
005300170420     C                   seton                                        lr
005400170412     C*
005500170412     C* Arresto il monitoring
005600170412     C                   endmon
005700100624     C*
005800170420     C                   return
005900010525     C*
006000010525     C*-----------------------------------------------------------------------------
006100170804
006200170804
006300170804
006400170804     C*
006500170804     C     exeInz        begsr
006600170804     C*
006700170804     C* Inizializzazioni
006800170804     C                   eval      oUBS717Esito = -1
006900170825     C                   clear                   oUBS717MSG
007000170804     C                   clear                   oUBS717SI95
007100170804     C                   clear                   oUBS717DLEN
007200170804     C                   clear                   oUBS717DATI
007300170804     C*
007400170804     C                   endsr
007500170824
007600170824
007700170824
007800170824     C*
007900170824     C     carTab        begsr
008000170824     C*
008100170824     C* Reperimento *OUT di DEFAULT "VASWS"
008200170824     C                   eval      iS713TBECOD = 'VPR'
008300170824     C                   eval      iS713TBEKE1 = '*VASWS'
008400170824     C                   eval      iS713TBEKE2 = '*DFTOUTQ'
008500170824     C                   eval      iS713TBELIN = *blanks
008600170824     C                   eval      iS713TBESIF = *blanks
008700170824     C                   call      'TIS713R'
008800170824     C                   parm                    iS713TBECOD
008900170824     C                   parm                    iS713TBEKE1
009000170824     C                   parm                    iS713TBEKE2
009100170824     C                   parm                    iS713TBELIN
009200170824     C                   parm                    iS713TBESIF
009300170824     C                   parm                    oS713TBEUNI
009400170824     C                   parm                    oS713ESITO
009500170824     C*
009600170824     C                   if        oS713ESITO = '1'
009700170824     C                   eval      dftOUTQ = %trim(oS713TBEUNI)
009800170824     C                   else
009900170824     C                   eval      dftOUTQ = 'VASSPOOL'
010000170824     C                   endif
010100170824     C*
010200170824     C* Reperimento TIPO FILE ETICHETTE SEGNACOLLO di DEFAULT "VASWS"
010300170824     C                   eval      iS713TBECOD = 'VPR'
010400170824     C                   eval      iS713TBEKE1 = '*VASWS'
010500170824     C                   eval      iS713TBEKE2 = '*TIPF-ETI'
010600170824     C                   eval      iS713TBELIN = *blanks
010700170824     C                   eval      iS713TBESIF = *blanks
010800170824     C                   call      'TIS713R'
010900170824     C                   parm                    iS713TBECOD
011000170824     C                   parm                    iS713TBEKE1
011100170824     C                   parm                    iS713TBEKE2
011200170824     C                   parm                    iS713TBELIN
011300170824     C                   parm                    iS713TBESIF
011400170824     C                   parm                    oS713TBEUNI
011500170824     C                   parm                    oS713ESITO
011600170824     C*
011700170824     C                   if        oS713ESITO = '1'
011800170824     C                   eval      dftTIPF_ETI = %trim(oS713TBEUNI)
011900170824     C                   else
012000170824     C                   eval      dftTIPF_ETI = 'SG'
012100170824     C                   endif
012200170824     C*
012300170824     C* Reperimento TIPO TRASFERIMENTO OUTPUT di DEFAULT "VASWS"
012400170824     C                   eval      iS713TBECOD = 'VPR'
012500170824     C                   eval      iS713TBEKE1 = '*VASWS'
012600170824     C                   eval      iS713TBEKE2 = '*TIP-TRASF'
012700170824     C                   eval      iS713TBELIN = *blanks
012800170824     C                   eval      iS713TBESIF = *blanks
012900170824     C                   call      'TIS713R'
013000170824     C                   parm                    iS713TBECOD
013100170824     C                   parm                    iS713TBEKE1
013200170824     C                   parm                    iS713TBEKE2
013300170824     C                   parm                    iS713TBELIN
013400170824     C                   parm                    iS713TBESIF
013500170824     C                   parm                    oS713TBEUNI
013600170824     C                   parm                    oS713ESITO
013700170824     C*
013800170824     C                   if        oS713ESITO = '1'
013900170824     C                   eval      dftTIP_TRASF = %trim(oS713TBEUNI)
014000170824     C                   else
014100170824     C                   eval      dftTIP_TRASF = 'BV'
014200170824     C                   endif
014300170824     C*
014400170824     C                   endsr
014500170824     C*
014600170804
014700170419
014800170419
014900170419     C*
015000170803     C     exeOpz        begsr
015100170803     C*
015200170803     C                   select
015300170803     C* Opzione STD
015400170803     C                   when      iUBS717OPZ = *blanks OR
015500170803     C                             iUBS717OPZ = '*STD'
015600170803     C                   exsr      exeOpzSTD
015700170803     C* Opzione "future"
015800170803     C                   other
015900170803     C                   endsl
016000170803     C*
016100170803     C                   endsr
016200170803
016300170824
016400170803
016500170803     C*
016600170803     C     exeOpzSTD     begsr
016700170804     C*
016800170804     C* Ridefinisco i dati spedizione ricevuti in input
016900170804     C                   eval      EDIVABDS = iUBS717VAB
017000170804     C*
017100170804     C* Ridefinisco in DS le opzioni di esecuzione richieste in input
017200170804     C                   eval      DVPC = iUBS717VPC
017300170804     C*
017400170804     C* Quindi in base alle opzioni richieste eseguo (nel ordine)
017500170804     C*
017600170804     C* - se richiesto Calcolo Instradamento Spedizione
017700170804     C                   if        §VPCCIS = 'S'
017800170804     C                   exsr      exeCIS_SI95
017900170804     C                   endif
018000170804     C*
018100170804     C* - se richiesto Assegnazione Segnacolli Spedizione
018200170804     C                   if        §VPCASG <> *blanks AND
018300170804     C                             §VPCASG <> 'N'
018400170804     C                   exsr      exeASG
018500170804     C                   endif
018600170804     C*
018700170804     C* - se richiesto Creazione Etichette Segnacollo
018800170804     C                   if        §VPCSGF <> *blanks
018900170804     C                   exsr      exeSGF_ETISPE
019000170804     C                   endif
019100170804     C*
019200170804     C* A questo punto esito è OK
019300170804     C                   eval      oUBS717Esito = 0
019400170804     C                   eval      iUBS717VAB   = EDIVABDS
019500170419     C*
019600170419     C                   endsr
019700170804
019800170804
019900170804
020000170804     C*
020100170804     C     exeCIS_SI95   begsr
020200170804     C*
020300170804     C                   clear                   UBSI95DS
020400170804     C                   eval      ISI95LIN = 'EN'
020500170804     C                   eval      ISI95TYP = 'F'                               C / V / F / *blnk
020600170804     C                   eval      ISI95CFD = §VPCCFD                           S / *blnk
020700170804     C                   eval      ISI95TLL = §VPCTLL                           S / *blnk
020800170804     C                   eval      ISI95IED = §VPCIED                           S / *blnk
020900170804     C                   eval      ISI95DAT = *zeros
021000170804     C*
021100170804     C                   clear                   RTNsi95esito
021200170804     C                   clear                   RTNsi95opcode
021300170804     C                   clear                   RTNsi95status
021400170804     C*
021500170804     C* Chiamata al driver di Calcolo Instradamento
021600170804     C                   call      'UBSI95R'
021700170804     C                   parm                    UBSI95DS
021800170804     C                   parm                    EDIVABDS
021900170804     C                   parm                    RTNsi95esito
022000170804     C                   parm                    RTNsi95opcode
022100170804     C                   parm                    RTNsi95status
022200170825     C*
022300170825     C* Valorizzo sempre il messaggio di ritorno
022400170825     C                   eval      oUBS717MSG = oSI95MSG
022500170825     C*
022600170804     C* Verifico esito chiamata, se errore => fatal error
022700170804     C                   if        RTNsi95esito = 0 AND
022800170804     C                             OSI95LNA > *zeros
022900170804     C                   eval      oUBS717SI95 = UBSI95DS
023000170804     C                   eval      vabLNA = OSI95LNA
023100170804     C                   eval      vabZNC = OSI95ZNC
023200170804     C                   else
023300170816     C                   eval      oUBS717Esito = -3
023400170804     C*                  return
023500170804     C                   endif
023600170804     C*
023700170804     C                   endsr
023800170804
023900170804
024000170804
024100170804     C*******************************
024200170804     C* Assegnazione Segnacolli Spedizione
024300170804     C*******************************
024400170804     C*
024500170804     C     exeASG        begsr
024600170804     C*
024700170804     C* Eseguo assegnazioe automatica segnacollo
024800170804     C                   eval      iAASGNLIN   = 'EN'
024900170804     C                   eval      iAASGNUNI   = iUBS717VPC
025000170804     C                   eval      iAASGNDSVAB = EDIVABDS
025100170804     C                   call      'UBAASGNR'
025200170804     C                   parm                    iAASGNLIN
025300170804     C                   parm                    iAASGNUNI
025400170804     C                   parm                    iAASGNDSVAB
025500170804     C                   parm                    oAASGNESI
025600170804     C                   parm                    oAASGNMSG
025700170825     C*
025800170825     C* Valorizzo sempre il messaggio di ritorno
025900170825     C                   eval      oUBS717MSG = oAASGNMSG
026000170804     C*
026100170804     C* Se in uscita reperimento KO => fatal error
026200170804     C                   if        oAASGNESI <> *zeros
026300170816     C                   eval      oUBS717Esito = -4
026400170804     C                   return
026500170804     C*
026600170804     C* Se tutto OK => recepisco le modifiche ai dati
026700170804     C                   else
026800170804     C                   eval      EDIVABDS = iAASGNDSVAB
026900170804     C                   endif
027000170804     C*
027100170804     C                   endsr
027200170804
027300170804
027400170804
027500170804     C*******************************
027600170804     C* Etichetta SEGNACOLLO - a livello di SPEDIZIONE
027700170804     C*******************************
027800170804     C*
027900170804     C     exeSGF_ETISPE begsr
028000170804     C*
028100170804     C* Inizializzazioni DS di procedure
028200170804     C                   clear                   UBBRTETIDS
028300170804     C                   clear                   UBSGU1IDS
028400170804     C                   clear                   UBSGU1DSA
028500170804     C*
028600170804     C* Valorizzo parametri per stampa segnacollo
028700170804     C                   select
028800170804     C                   when      §VPCSGF = 'P'
028900170804     C                   eval      UBBRTETIDS.PIN_TPSTP = 'A'
029000170804     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'PDF'
029100170804     C                   when      §VPCSGF = 'T'
029200170804     C                   eval      UBBRTETIDS.PIN_TPSTP = 'A'
029300170804     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'TIF'
029400170804     C                   when      §VPCSGF = 'Z'
029500170804     C                   eval      UBBRTETIDS.PIN_TPSTP = 'Z'
029600170804     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'TXT'
029700171023     C                   when      §VPCSGF = 'L'
029800171023     C                   eval      UBBRTETIDS.PIN_TPSTP = 'A'
029900171023     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'PCL'
030000170804     C                   other
030100170804     C                   eval      UBBRTETIDS.PIN_TPSTP = 'Z'
030200170804     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'TXT'
030300170804     C                   endsl
030400170804     C*
030500170804     C                   select
030600170804     C                   when      iUBS717ENV = 'VWS'                           * ASCII
030700170804     C                   eval      UBSGU1DSA.ISGU1ADSTO = 'DATI'
030800170804     C                   when      iUBS717ENV = 'VWB'                           * BASE64
030900170804     C                   eval      UBSGU1DSA.ISGU1ADSTO = 'BAS64'
031000170804     C                   other
031100170804     C                   eval      UBSGU1DSA.ISGU1ADSTO = 'DATI'
031200170804     C                   endsl
031300170804     C*
031400170824     C                   eval      UBBRTETIDS.PIN_OUTQ  = dftOUTQ
031500170804     C                   eval      UBBRTETIDS.PIN_TLA   = *blanks
031600170804     C                   eval      UBBRTETIDS.PIN_OFSX  = %dec(§VPCOFX:8:3)
031700170804     C                   eval      UBBRTETIDS.PIN_OFSY  = %dec(§VPCOFY:8:3)
031800170804     C                   eval      UBBRTETIDS.PIN_BORDO = §VPCBRD
031900170804     C                   eval      UBBRTETIDS.PIN_LOGO  = §VPCPDL
032000170804     C                   eval      UBBRTETIDS.PIN_ROW   = §VPCROW
032100170804     C                   eval      UBBRTETIDS.PIN_TIE   = *blanks
032200170804     C                   eval      UBBRTETIDS.PIN_RST   = *blanks
032300170804     C*
032400170804     C* Valorizzo parametri per driver utilità segnacolli 1 - Formato "A"
032500170804     C                   eval      UBSGU1DSA.ISGU1AOPZ  = *blanks
032600170804     C                   eval      UBSGU1DSA.ISGU1AKSU  = iUBS717KSU
032700170804     C                   eval      UBSGU1DSA.ISGU1AUNI  = iUBS717VPC
032800170824     C                   eval      UBSGU1DSA.ISGU1ATIPF = dftTIPF_ETI
032900170824     C                   eval      UBSGU1DSA.ISGU1ATIPT = dftTIP_TRASF
033000170804     C                   eval      UBSGU1DSA.ISGU1AMSKF = §VPCSGN
033100170804     C                   eval      UBSGU1DSA.ISGU1ADATI = EDIVABDS
033200170804     C                   eval      UBSGU1DSA.ISGU1ADLEN = %size(EDIVABDS)
033300170804     C*
033400170804     C* Valorizzo parametri per driver utilità segnacolli 1
033500170804     C                   eval      UBSGU1IDS.ISGU1ILIN  = 'EN'
033600170804     C                   eval      UBSGU1IDS.ISGU1IFMT  = 'A'
033700170804     C                   eval      UBSGU1IDS.ISGU1IFLAT = UBSGU1DSA
033800170804     C                   eval      UBSGU1IDS.ISGU1IFLEN = %size(UBSGU1DSA)
033900170804     C                   eval      UBSGU1IDS.ISGU1IBRTE = UBBRTETIDS
034000170804     C                   eval      UBSGU1IDS.ISGU1IBLEN = %size(UBBRTETIDS)
034100170804     C*
034200170804     C* Chiamata al driver stampa e reperimento etichetta segnacolli
034300170804     C                   call      'UBSGU1R'
034400170804     C                   parm                    UBSGU1IDS
034500170804     C                   parm                    UBSGU1OLEN
034600170804     C                   parm                    UBSGU1OUT
034700170825     C*
034800170825     C* Valorizzo sempre il messaggio di ritorno
034900170825     C                   eval      oUBS717MSG = UBSGU1IDS.OSGU1IMSG
035000170804     C*
035100170804     C* Se errore => DUMP e uscita con errore
035200170804     C                   if        UBSGU1IDS.OSGU1IESI <> *zeros
035300170816     C                   eval      oUBS717Esito = -5
035400170804     C                   return
035500170804     C*
035600170804     C* Se tutto OK => recepisco output
035700170804     C                   else
035800170804     C                   eval      oUBS717DLEN = UBSGU1OLEN
035900170804     C                   eval      oUBS717DATI = UBSGU1OUT
036000170804     C                   endif
036100170804     C*
036200170804     C                   endsr
036300170804
036400050920
036500050920
036600000714      /TITLE Routine di *init PGM
036700000714     C*
036800000714     C     *inzsr        begsr
036900060103     C*
037000170803     C     *entry        plist
037100170803     C                   parm                    iUBS717VAB
037200170803     C                   parm                    iUBS717ENV
037300170803     C                   parm                    iUBS717OPZ
037400170803     C                   parm                    iUBS717KSU
037500170804     C                   parm                    iUBS717VPC
037600170803     C                   parm                    oUBS717Esito
037700170825     C                   parm                    oUBS717MSG
037800170803     C                   parm                    oUBS717SI95
037900170804     C                   parm                    oUBS717DLEN
038000170803     C                   parm                    oUBS717DATI
038100170803     C*
038200000714     C                   endsr
038300000714
