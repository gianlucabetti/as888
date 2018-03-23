000100000000     H
000200000000     F*-------------------------------------------------------------*
000300940824     F*- Controllo P.Iva Europea                            *
000400940824     F*-------------------------------------------------------------*
000500050726      *
000600050726      * ATTENZIONE al programma ora vengono passati 2 parametri
000700050726      *            il secondo viene passato solo da alcuni programmi
000800050907      *            questo secondo parametro serve per il controllo
000900050726      *            della partita iva di un cliente di San Marino
001000050726      *            il secondo parametro passato contiene la provincia
001100040115      *
001200941003     F* ERRORI:
001300061106     F* IVAFLG = -9 ERRORE ANCHE FORZABILE-->COD ISO P.IVA INCONGR NAZ
001400941003     F* IVAFLG = -5 ERRORE NON   FORZABILE-->NAZIONE ERRATA
001500941003     F* IVAFLG = -3 ERRORE NON   FORZABILE-->MANCA RECORD P.IVA IN DS4I
001600941003     F* IVAFLG = -2 ERRORE NON   FORZABILE-->ERRATA LA LUNGHEZZA
001700061106     F* IVAFLG = -1 ERRORE NON   FORZABILE--> ERRATA LA FORMALITA'
001800941003     F*
001900000000     F*-------------------------------------------------------------*
002000940228     FTABEL00F  IF   E           K DISK
002100940228     D*-------------------------------------------------------------*
002200940228     D*- Schiera per ricerca lunghezza della stringa               -*
002300940228     D*-------------------------------------------------------------*
002400940823     D EIVA            S              1    DIM(14)
002500080421     D cIVA            S              1    DIM(14)
002600100115     D MSG             S             78    DIM(8) CTDATA PERRCD(1)
002700080421     D LET             S              1    DIM(32) CTDATA PERRCD(32)
002800080421     D LEM             S              1    DIM(32) CTDATA PERRCD(32)
002900080421     D NUM             S              1    DIM(10) CTDATA PERRCD(10)
003000950717     D K78             S              1    DIM(78)
003100940228     D*-------------------------------------------------------------*
003200940301     D XIVAEU        E DS
003300940301     D*
003400940930     D DS15          E DS
003500940930     D DS4I          E DS
003600050726
003700050726     d wpos            s              2  0
003800080311     d wforzaITA       s              1
003900080421     d wivapar         s                   like(ivapar)
004000940301     C*-------------------------------------------------------------*
004100940301     C*  Ciclo Principale
004200940301     C*-------------------------------------------------------------*
004300940301     C     *ENTRY        PLIST
004400940301     C                   PARM                    XIVAEU
004500050726     c                   Parm                    xivaprv           2
004600080311     c                   Parm                    xforzaITA         1
004700080311     c
004800080311     c                   if        %parms=3
004900080311     c                   eval      wforzaita=xforzaita
005000080311     c                   else
005100080311     c                   eval      Wforzaita=' '
005200080311     c                   endif
005300941003     C*
005400941003     C                   CLEAR                   IVAFLG
005500941003     C                   CLEAR                   IVAMSG
005600941003     C*
005700941003     C* TIPO LANCIO = "C"           CHIUSO CON LR IL FILE
005800941003     C*             = "L" ELABORO E CHIUDO CON LR
005900941003     C*             = " " ELABORO E CHIUDO CON RETRN
006000940930     C*
006100941003    1C     IVATLA        IFEQ      'C'
006200941003     C                   SETON                                        LR
006300941003   X1C                   ELSE
006400941003     C*
006500940930     C* REPERISCO DALLA TABELLA NAZIONE SE E' NAZIONE ITALIA
006600940930     C                   MOVE      '15'          TBWCOD
006700941003     C                   MOVEL(P)  IVANAR        TBWKEY
006800940930     C*
006900940930     C     KEYTAB        CHAIN     TABEL00F                           80
007000941003     C* ERRORE --> NAZIONE ERRATA
007100941003    2C     *IN80         IFEQ      *ON
007200941003     C                   MOVEL     MSG(5)        IVAMSG
007300941003     C                   Z-SUB     5             IVAFLG
007400941003     C                   GOTO      ENDCTR
007500941003     C                   ELSE
007600941003     C                   MOVEL     TBLUNI        DS15
007700941003    2C                   ENDIF
007800950116     C*
007900950116     C* SE IVASTA ='$$'   ----> NESSUN CONTROLLO
008000950616     C     IVASTA        IFEQ      '$$'
008100061123     C**!!!§15ISO        OREQ      '$$'
008200950616     C* SE PRIVATO LA P.IVA DEVE ESSERE VUOTA
008300950626     c* o uguale alla scritta privato
008400950616     C     IVAPAR        IFNE      *BLANKS
008500950626     C     'PRIVATO '    SCAN      IVAPAR                                 99
008600950626     C     *IN99         IFEQ      *OFF
008700950616     C                   MOVEL     MSG(6)        IVAMSG
008800950616     C                   Z-SUB     6             IVAFLG
008900950616    2C                   ENDIF
009000950626    2C                   ENDIF
009100950616     C                   GOTO      ENDCTR
009200950616    2C                   ENDIF
009300050726
009400050726      * se passato anche il secondo parametro (provincia) controllo se p.i. di San Marino
009500050726      * in questo caso non devo fare nessun controllo
009600080421     c                   clear                   wSM               2            2
009700050726if  2c                   If        %Parms > 1
009800050726      * nazione italia
009900050726     c                             and ivanar = *Blanks
010000050726      * provincia FO o RN
010100061025     c                             and (xivaprv = 'FO' or xivaprv = 'RN'
010200061025     c                                  or xivaprv = 'FC')
010300050726      * controllo se nella partita iva c'è la sigla SM x San Marino
010400050726     c                   Clear                   wpos
010500050726     c                   Eval      wpos = %scan('SM':ivapar)
010600080421      * controlli per san Marino
010700070426if  3c                   If        wpos > *Zeros or ivasta = 'SM'
010800050907     c                   ExSr      Sr_Ctrsm
010900080421     c* Se trovato errore --> esco
011000080421     c                   if        ivaflg<0
011100080421     c                   goto      endctr
011200080421     c                   endif
011300080421     c
011400080421     c                   eval      wSM='SM'
011500080421     C                   MOVEA     wIVAPAR       EIVA
011600070426    3c                   EndIf
011700050726    2c                   EndIf
011800950116     C*
011900080421   1ac                   if        wSM='  '
012000080421     c
012100950116     C* SE NON IMMESSO IL CODICE ISO LO PRENDO
012200950116     C     IVASTA        IFEQ      *BLANKS
012300950116    2C     §15ITA        ANDNE     'I'
012400950116     C                   MOVEL     §15ISO        IVASTA
012500950116    2C                   ENDIF
012600941121     C*
012700941121     C*  Reperisco sulla tabella la lunghezza della stringa
012800941121     C                   CLEAR                   DS4I
012900941121     C                   MOVE      '4I'          TBWCOD
013000941121     C                   MOVEL(P)  IVASTA        TBWKEY
013100941121     C     KEYTAB        CHAIN     TABEL00F                           80
013200941121     C*
013300941121     C* ERRORE --> NON ESISTE LA TABELLA DELLA P.IVA
013400941121    2C     *IN80         IFEQ      *ON
013500941121     C     TBLFLG        ORNE      *BLANKS
013600941121     C                   MOVEL     MSG(3)        IVAMSG
013700941121     C                   Z-SUB     3             IVAFLG
013800941121     C                   GOTO      ENDCTR
013900941121   x2C                   ELSE
014000941121     C                   MOVEL     TBLUNI        DS4I
014100941121    2C                   ENDIF
014200941122     C*
014300940301     C                   MOVEA     IVAPAR        EIVA
014400080421   1aC                   ENDIF
014500940824     C*
014600940301     C*  Determino la lunghezza effettiva della partita IVA
014700940301     C                   Z-ADD     1             X                 2 0
014800940301     C     ' '           LOOKUP    EIVA(X)                                40
014900941003    3C     *IN40         IFEQ      '0'
015000940823     C                   Z-ADD     14            LUNGH             2 0
015100940301     C                   ELSE
015200940301     C     X             SUB       1             LUNGH
015300941003    3C                   END
015400940823     C*
015500940301     C*  Se la lunghezza effettiva della stringa è diversa da
015600940301     C*  quella prevista gestisco errore
015700941003    3C     LUNGH         IFNE      §4ICHR
015800040630    3C     LUNGH         ANDNE     §4ICH2
015900040630    3C     LUNGH         ANDNE     §4ICH3
016000071102     c* non accetto lunghezza =0
016100071102     c     LUNGH         oreq      0
016200071102     c
016300950717     C                   MOVEA     MSG(2)        K78
016400950717     C                   MOVEL     §4ICHR        W002A             2
016500950717     C                   MOVEA     W002A         K78(47)
016600950717     C                   MOVEA     K78           IVAMSG
016700080421     c
016800080421     c                   if        wSM='  '
016900040630     C                   EVAL      IVAMsg = 'Il codice IVA della nazione ' +
017000040630     C                             IVASta + ' può essere lungo ' +
017100040630     C                             %TRIML(%EDITC(§4IChr:'Z'))
017200080421     c                   else
017300080421     c* San Marino
017400080421     c                   Eval      ivamsg = 'La partita IVA di San Marino deve-
017500080421     c                              essere: SM più ' +
017600080421     c                             %triml(%editc(§4ichr:'Z'))
017700080421     c                   endif
017800080421     c
017900040630     C                   IF        §4ICh2 > 0
018000040630     C                   EVAL      IVAMsg = %TRIMR(IVAMsg) + ', ' +
018100040630     C                             %TRIML(%EDITC(§4ICh2:'Z'))
018200040630     C                   ENDIF
018300040630     C                   IF        §4ICh3 > 0
018400040630     C                   EVAL      IVAMsg = %TRIMR(IVAMsg) + ', ' +
018500040630     C                             %TRIML(%EDITC(§4ICh3:'Z'))
018600040630     C                   ENDIF
018700040630     C                   EVAL      IVAMsg = %TRIMR(IVAMsg) + ' caratteri.'
018800941003     C                   Z-SUB     2             IVAFLG
018900941003     C                   GOTO      ENDCTR
019000080421     c
019100941003   X3C                   ELSE
019200940823     C*
019300940823     C* SE P.IVA ITALIANA ESEGUO ANCHE IL CONTROLLO FORMALE
019400061113    4C**!!!§15ITA        IFEQ      'I'
019500061113      * controllo il codice iso e non la nazione italia perchè posso inserire una
019600061113      * partita iva straniera su cliente italia
019700061113    4c     §4iITA        IFEQ      'I'
019800940823     C                   MOVEL     IVAPAR        XPIVA
019900940824     C                   CLEAR                   XSTATO
020000940823     C*
020100981124     C                   CALL      'XPARIVAO'
020200940823     C                   PARM                    XPIVA            16
020300940824     C                   PARM                    XSTATO            1 0
020400940824     C*
020500940824     C                   Z-ADD     XSTATO        IVAFLG
020600941003     C* ERRORE --> P.IVA ITALIANA FORMALMENTE ERRATA
020700941003    5C     IVAFLG        IFEQ      -1
020800941003     C                   MOVEL     MSG(1)        IVAMSG
020900941003     C                   GOTO      ENDCTR
021000941003    5C                   ENDIF
021100941003    4C                   ENDIF
021200941003     C*
021300941003    3C                   ENDIF
021400100115     c
021500100115     c* Se nella tabella 4I c'e' la struttura del codice IVA, controllo
021600100115     c                   if        §4ipar<>*blanks
021700100115     c                   exsr      chkfor
021800100115     c                   endif
021900100115     c
022000100115     c                   if        wSM='SM' and wpos>0
022100100115     c                   clear                   ivasta
022200100115     c                   endif
022300040115     C*
022400040115     C* SE CODICE ISO DELLA P.IVA INCONGRUENTE CON CODICE ISO DELLA NAZ
022500040115     C*  --> ERRORE FORZABILE
022600080312    3C***  §15ISO        IFNE      IVASTA
022700080312     C***  §4IITA        ANDNE     §15ITA
022800080312     c***  §15iso        oreq      '$$'
022900080421    3c                   if        wSM='  '
023000080421    4c                   if        (§15iso<>ivasta and (§4iita<>'I' or
023100080312     c                             §15ita<>'I')) or §15iso='$$'
023200080311     c
023300100115     c* In filiale errore non forzabile se nazione Italia
023400080421    5c                   if        §15ita='I' and wforzaita=' '
023500080311     C                   MOVEL     MSG(7)        IVAMSG
023600080311     C                   Z-SUB     1             IVAFLG
023700080311     C                   GOTO      ENDCTR
023800080311     c                   else
023900100115     c
024000100115     c* Se nazione ITALIA messaggio diverso
024100100115     c                   if        ivanar=*blanks
024200100115     C                   MOVEL     MSG(8)        IVAMSG
024300100115     c                   else
024400100115     C                   MOVEL     MSG(4)        IVAMSG
024500100115     c                   eval      %subst(ivamsg:47:2) = §15iso
024600100115     c                   endif
024700100115     c
024800061106     C                   Z-SUB     9             IVAFLG
024900040115     C                   GOTO      ENDCTR
025000080421    5C                   ENDIF
025100080421    4C                   ENDIF
025200080421    3C                   ENDIF
025300941003     C*
025400941003     C     ENDCTR        TAG
025500940823     C*
025600941003     C* SE IVATLA = "L" CHIUDO CON LR
025700941003    2C     IVATLA        IFEQ      'L'
025800941003     C                   SETON                                        LR
025900941003   X2C                   ELSE
026000941003     C                   RETURN
026100941003    2C                   ENDIF
026200941003    1C                   ENDIF
026300050907
026400050907      *-------------------------------------------------------------*
026500050907      * Controllo la lunghezza della partita iva di S.Marino
026600050907      *-------------------------------------------------------------*
026700050907     c     Sr_Ctrsm      BegSr
026800080421     c                   clear                   wivapar
026900080421     c
027000080421     c                   if        ivasta<>*blanks and wpos>0
027100080421     c                   Z-sub     2             ivaflg
027200080421     c                   Eval      ivamsg = 'Codice ISO "SM" ripetuto 2 volte'
027300080421     c                   leavesr
027400080421     c                   endif
027500080421
027600080421     c                   if        ivasta=*blanks and wpos>1
027700080421     c                   Z-sub     2             ivaflg
027800080421     c                   Eval      ivamsg = 'Codice ISO "SM" deve essere nella +
027900080421     c                              prima posizione o nel campo del codice ISO'
028000080421     c                   leavesr
028100080421     c                   endif
028200080421
028300080421     c                   if        ivasta=*blanks and wpos=1
028400080421     c                   eval      ivasta='SM'
028500080421     c                   eval      wivapar=%subst(ivapar:3:12)
028600080421     c                   endif
028700080421
028800080421     c                   if        wpos=0
028900080421     c                   eval      wivapar=ivapar
029000080421     c                   endif
029100080421     c
029200050907
029300050907      * Aggancio la tabella 4I per recuperare la lunghezza
029400050907     c                   Clear                   ds4i
029500050907     c                   Eval      tbwcod = '4I'
029600050907     c                   Eval      tbwkey = 'SM'
029700050907     c     keytab        Chain     Tabel00f
029800050907     c                   If        %Found(Tabel00f) and tblflg = *Blanks
029900050907     c                   Eval      ds4i = tbluni
030000050907      * Se non trovo la tabella o se è annullata chiodo la lunghezza a 5
030100050907     c                   Else
030200050907     c                   Eval      §4ichr = 5
030300050907     c                   EndIf
030400050907
030500050907     c                   EndSr
030600080421     C*------------------------------------------------------------------------*
030700080421     C* CHKFOR - CONTROLLO FORMALE DEL codice IVA se presente
030800080421     C*------------------------------------------------------------------------*
030900080421     C     CHKFOR        BEGSR
031000080421     C*---
031100080421     C* RIASSUMO I CARATTERI DI CONTROLLO PREVISTI;
031200080421     C*  "N" - SOLO NUMERI
031300080421     C*  "L" - SOLO LETTERE
031400080421     C*  "A" - CAMPO ALFANUMERICO     (NUMERI LETTERE)
031500080421     C*  " " - CAMPO DA NON IMMETTERE
031600080421     C*  "O" - OPZIONALE ALFANUMERICO (NUMERI LETTERE E BLANK)
031700080421     C*  "Q" - OPZIONALE SOLO LETTERE (       LETTERE E BLANK)
031800080421     C*  "0" - OPZIONALE NUMERICO     (NUMERI         E BLANK)
031900080421     C*---
032000080421     c                   setoff                                       90
032100080421     C                   MOVEA     §4ipar        cIVA
032200080421     c                   z-add     1             xx                3 0
032300080421    1c                   dow       xx <=14  and not *in90
032400080421     C*
032500080421     C* CARATTERE  D A   N O N   I M M E T T E R E
032600080421    2c                   if        Civa(xx)=' ' and eiva(xx)<>' '
032700080421     c                   seton                                        90
032800080421    2c                   endif
032900080421     C*
033000080421     C* CARATTERE  N U M E R I C O
033100080421    2c                   if        Civa(XX)='N' or Civa(XX)='A'   or
033200080421     c                             Civa(XX)='O' or Civa(XX)='0'
033300080421     c     Eiva(XX)      lookup    NUM                                    30
033400080421     c
033500080421     C* 30 OFF- NON TROVATO: PER "N" ERRORE SUBITO
033600080421    3c                   if        not *in30
033700080421     c                   eval      *in90=*on
033800080421     c
033900080421     C* 30 OFF- NON TROVATO: PER "0" ERRORE SE NON E' NEMMENO ' '
034000080421    4c                   if        Civa(XX)='0' and Eiva(XX)=' '
034100080421     c                   eval      *in90=*off
034200080421    4c                   endif
034300080421    3c                   endif
034400080421    2c                   endif
034500080421     C*
034600080421     C* CARATTERE  A L F A N U M E R I C O
034700080421    2c                   if        (*in90 and Civa(XX)='A') or
034800080421     c                             (*in90 and Civa(XX)='O') or
034900080421     c                             Civa(XX)='L' or
035000080421     c                             Civa(XX)='Q'
035100080421     c     Eiva(XX)      lookup    let                                    30
035200080421     C*---
035300080421     C* 30 OFF- NON TROVATO: PER "L" ERRORE SUBITO
035400080421     C* 30 OFF- NON TROVATO: PER "A" ERRORE SUBITO
035500080421     C*                      XCHE' NON TROVATO NEMMENO NEI NUMERICI
035600080421    3c                   if        not *in30
035700080421     c                   eval      *in90=*on
035800080421    3c                   endif
035900080421    3c                   if            *in30
036000080421     c                   eval      *in90=*off
036100080421    3c                   endif
036200080421     c
036300080421    3c                   if        *in90 and Eiva(XX)=' '
036400080421    4c                   if        Civa(XX)='O'  or Civa(XX)='Q'
036500080421     c                   eval      *in90=*off
036600080421    4c                   endif
036700080421    3c                   endif
036800080421    2c                   endif
036900050907
037000080421     c* 90 on - errore formale del cap
037100080421    2c                   if        *in90
037200080421     c                   Z-sub     2             ivaflg
037300080421     c                   Eval      ivamsg = 'Codice IVA formalmente errato'
037400080421    2c                   endif
037500080421     c
037600080421     c                   add       1             xx
037700080421     c                   enddo
037800080421     c
037900080421     c                   ENDSR
038000940301     C*-------------------------------------------------------------*
038100940301     C     *INZSR        BEGSR
038200940301     C*-------------------------------------------------------------*
038300940301     C*  Definizione della chiave di accesso alla tabella 4I
038400940301     C     KEYTAB        KLIST
038500940301     C                   KFLD                    TBWKUT
038600940301     C                   KFLD                    TBWCOD
038700940301     C                   KFLD                    TBWKEY
038800940301     C*  Definizione Varibili
038900940301     C     *LIKE         DEFINE    TBLKUT        TBWKUT
039000940301     C     *LIKE         DEFINE    TBLCOD        TBWCOD
039100940301     C     *LIKE         DEFINE    TBLKEY        TBWKEY
039200940301     C*  Inizializzazione
039300940301     C                   Z-ADD     1             TBWKUT
039400940301     C                   ENDSR
039500950626**
039600941003Partita IVA italiana formalmente errata
039700950717Lunghezza Partita IVA errata: e' composta di  xx caratteri
039800941003Codice ISO inesistente o non prevista tabella della Partita IVA
039900100115Cod.ISO della P.IVA incongruente con cod.ISO "XX" della nazione
040000941003Nazione Errata
040100950626Se Privato -codice ISO $$ -lasciare Part.IVA in bianco o uguale a "PRIVATO"
040200080311Per nazione ITALIA non ammessa una partita IVA estera
040300100115Immessa una partita IVA estera per nazione ITALIA.
040400080421**
040500080421ABCDEFGHIJKLMNOPQRSTUVWXYZOAUEEI
040600080421**
040700080421abcdefghijklmnopqrstuvwxyzòàùèéì
040800080421**
0409000804210123456789
