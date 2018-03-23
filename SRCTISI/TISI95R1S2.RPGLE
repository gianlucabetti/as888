000100090717     h datedit(*ymd/) dftactgrp(*no) actgrp(*caller)
000200080620      //********************************************************************************************
000300100114     Ftabel00f  if   e           k disk
000400090811     F*
000500100504     D MSGORI          S             80    DIM(30) CTDATA PERRCD(1)
000600100504     D MSGITA          S            100    DIM(30) CTDATA PERRCD(1)
000700100504     D MSGENG          S            100    DIM(30) CTDATA PERRCD(1)
000800100504     D MSGFRA          S            100    DIM(30) CTDATA PERRCD(1)
000900100504     D MSGDEU          S            100    DIM(30) CTDATA PERRCD(1)
001000091027     D*
001100090810     D tisi95ds      e ds
001200100504     D tisi97ds      e ds
001300100504     D dcppflo       e ds
001400100114     D dsT5          e ds
001500100430     D ds15          e ds
001600090810     D*
001700090810     D jMsg            S              2  0
001800091027     D*
001900080620
002000091027     D/COPY GAITRASRC/SRCPROTOPI,TISI95R1
002100091027
002200091027     D*-------------------
002300091027     D* COSTANTI
002400091027     D*-------------------
002500091027     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
002600091027     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
002700091027
002800091027
002900080620      //********************************************************************************************
003000080620      //
003100080620      // Definizione parametri procedura.
003200080620      //
003300080620      //********************************************************************************************
003400080620     C     *ENTRY        PLIST
003500091027     C                   PARM                    tisi95r1ds
003600100114     C*
003700100114     C* Chiave su TABEL00F - Completa
003800100114     C     KEYtab_C      KLIST
003900100114     C                   KFLD                    tblKUT
004000100114     C                   KFLD                    tblCOD
004100100114     C                   KFLD                    tblKEY
004200091027     C*
004300091027     C* Verifica parametri di input
004400091027     C                   exsr      chkPARIN
004500090810     C*
004600090810     C* Calcolo instradamento
004700090810     C                   exsr      calINSTR
004800090810     C*
004900090810     C* Valorizzazione finale parametri d output
005000090810     C                   exsr      valPAROUT
005100090810     C*
005200090810     C                   return
005300091027
005400091027
005500091027      /TITLE Verifica parametri di input
005600091027     C     chkPARIN      BEGSR
005700091027     C*
005800091027     C                   if        tisi95r1ds.si95ilang = *blanks
005900091027     C                   eval      tisi95r1ds.si95ilang = 'IT'
006000091027     C                   else
006100091027     C                   eval      tisi95r1ds.si95ilang =
006200091027     C                                   %xlate(minu:maiu:tisi95r1ds.si95ilang)
006300091027     C                   endif
006400091027     C*
006500091027     C                   ENDSR
006600090810
006700090810
006800090810      /TITLE Calcolo instradamento
006900090810     C     calINSTR      BEGSR
007000100805     C*
007100100805     C* Inizializzazioni
007200100805     C                   clear                   dcppflo
007300100805     C                   clear                   tisi97ds
007400091027     C*
007500100805     C* passagggio parametri
007600091027     C                   eval      tisi95ds = tisi95r1ds.tisi95ds
007700100416     C*
007800100416     C* <<< CONTROLLI PRIMARI >>>
007900100416     C*
008000100416     C* >>> CONTROLLI PRIMARI <<<
008100091027     C*
008200100416     C* Procedo solo se nn presenti errori nei controlli PRIMARI
008300100416     C                   if        o95msg = *blanks
008400100416     C*
008500091027     C                   call      'TISI95R'
008600091027     C                   parm                    tisi95ds
008700100331     C*
008800100331     C* I controlli seguenti sono subordinati alla chiamata x "calcolo instradamento"
008900100331     C                   if        tisi95r1ds.si95itype = 'C'
009000100507     C*
009100100507     C* Verifiche Italia <=> network estero
009200100507     C                   if        o95err = *blanks
009300100507     C                   if        i95nar  = *blanks AND
009400100507     C                             i95fi1 <> *blanks
009500110801     C                   eval      o95err = 'T'
009600100507     C                   eval      o95msg = MSGORI(25)
009700100507     C                   endif
009800100507     C                   endif
009900100507     C*
010000100507     C* Verifiche estero <=> network Italia
010100100507     C                   if        o95err = *blanks
010200100507     C                   if        i95nar <> *blanks AND
010300100507     C                             i95fi1  = *blanks
010400110801     C                   eval      o95err = 'T'
010500100507     C                   eval      o95msg = MSGORI(25)
010600100507     C                   endif
010700100507     C                   endif
010800100507     C*
010900100507     C* Verifiche estero <=> network esteri
011000100507     C                   if        o95msg = *blanks
011100100507     C                   if        i95nar <> *blanks AND
011200100507     C                             i95fi1 <> *blanks
011300100507     C                   eval      tblKUT = 1
011400100507     C                   eval      tblCOD = '15'
011500100507     C                   eval      tblKEY = i95nar
011600100507     C     KEYtab_C      chain     tabel00f
011700100507     C                   if        %found(tabel00f)
011800100507     C                   eval      ds15 = tblUNI
011900100507     C                   select
012000100507     C                   when      i95fi1 = 'E' and §15EEX <> 'S'
012100110801     C                   eval      o95err = 'T'
012200100507     C                   eval      o95msg = MSGORI(25)
012300100507     C                   when      i95fi1 = 'D' and §15LAD  = *zeros
012400110801     C                   eval      o95err = 'T'
012500100507     C                   eval      o95msg = MSGORI(25)
012600100507     C                   when      i95fi1 = 'S' and §15LAF  = *zeros
012700110801     C                   eval      o95err = 'T'
012800100507     C                   eval      o95msg = MSGORI(25)
012900100507     C                   endsl
013000100507     C                   endif
013100100507     C                   endif
013200100507     C                   endif
013300100504     C*
013400100504     C* Verifico Utilizzi Particolari CAP
013500100504     C                   if        o95err = *blanks
013600100504     C                   select
013700100504     C                   when      i95fi1 = 'E'
013800100504     C                   eval      i97ntw = 'EEX'
013900100504     C                   when      i95fi1 = 'D'
014000100504     C                   eval      i97ntw = 'DPD'
014100100504     C                   when      i95fi1 = 'S'
014200100504     C                   eval      i97ntw = 'FED'
014300100504     C                   endsl
014400100504     C                   call      'TISI97R'
014500100504     C                   parm                    tisi95ds
014600100504     C                   parm                    tiSI97ds
014700100504     C                   if        o97err = ' ' AND o97uti = 'N'
014800100504     C                   eval      dcppflo = o97flo
014900100504     C                   select
015000100504     C* No tutte spedizioni...
015100100504     C                   when      §cppall='S'
015200110801     C                   eval      o95err = 'U'
015300100504     C                   eval      o95msg = MSGORI(26)
015400100504     C* No spedizioni in contrassegno...
015500100504     C                   when      §cppcas = 'S' and i95fca = 'S'
015600110801     C                   eval      o95err = 'V'
015700100504     C                   eval      o95msg = MSGORI(27)
015800100504     C* No spedizioni in porto assegnato...
015900100504     C                   when      §cpppas <> ' ' and i95tpo = 'A'
016000110801     C                   eval      o95err = 'W'
016100100504     C                   eval      o95msg = MSGORI(28)
016200100504     C                   other
016300100504     C* No spedizioni "altro"...
016400110801     C***                eval      o95err = 'X'
016500100504     C***                eval      o95msg = MSGORI(29)
016600100504     C                   endsl
016700100504     C                   endif
016800100504     C                   endif
016900100114     C*
017000100114     C* Verifiche specifiche x tipo servizio "H"
017100100114     C                   if        o95err = *blanks
017200100114     C                   if        i95tsp = 'H' and
017300100114     C                             o95ttc < 1000
017400110801     C                   eval      o95err = 'P'
017500100114     C                   eval      o95msg = MSGORI(21)
017600100114     C                   endif
017700100114     C                   endif
017800100114     C*
017900100114     C* Verifica parametri "TISI57" - Limiti x tipo servizio
018000100114     C                   if        o95msg = *blanks
018100100114     C                   eval      tblKUT = 1
018200100114     C                   eval      tblCOD = 'T5'
018300100114     C                   eval      tblKEY = i95tsp
018400100114     C     KEYtab_C      chain     tabel00f
018500100114     C                   if        %found(tabel00f)
018600100114     C                   eval      dsT5 = tblUNI
018700100114     C                   if        tisi95r1ds.si95incl > §T5NCL OR
018800100114     C                             i95lkg              > §T5PKG OR
018900100114     C                             i95lmc              > §T5VLM
019000110801     C                   eval      o95err = 'Q'
019100100114     C                   eval      o95msg = MSGORI(22)
019200100114     C                   endif
019300100114     C                   endif
019400100114     C                   endif
019500100114     C*
019600100114     C* Verifica parametri "TISI57" - Limiti x network
019700100114     C                   if        o95msg = *blanks
019800100114     C                   eval      tblKEY = *blanks
019900100504     C                   select
020000100504     C                   when      i95fi1 = 'E'
020100100504     C                   eval      tblKEY = 'EEX'
020200100504     C                   when      i95fi1 = 'D'
020300100114     C                   eval      tblKEY = 'DPD'
020400100504     C                   when      i95fi1 = 'S'
020500100114     C                   eval      tblKEY = 'FED'
020600100504     C                   endsl
020700100114     C                   eval      tblKUT = 1
020800100114     C                   eval      tblCOD = 'T5'
020900100114     C     KEYtab_C      chain     tabel00f
021000100114     C                   if        %found(tabel00f)
021100100114     C                   eval      dsT5 = tblUNI
021200100114     C                   if        tisi95r1ds.si95incl > §T5NCL OR
021300100114     C                             i95lkg              > §T5PKG OR
021400100114     C                             i95lmc              > §T5VLM
021500110801     C                   eval      o95err = 'R'
021600100114     C                   eval      o95msg = MSGORI(23)
021700100114     C                   endif
021800100114     C                   endif
021900100114     C                   endif
022000100115     C*
022100100115     C* Verifiche network <=> estero
022200100126     C                   if        o95msg = *blanks
022300100115     C                   if        o95lna =  *zeros AND
022400100115     C                             o95znc =  *zeros
022500100115     C                   if        i95nar <> *blanks OR
022600100115     C                             i95fi1 <> *blanks
022700110801     C                   eval      o95err = 'T'
022800100115     C                   eval      o95msg = MSGORI(25)
022900100115     C                   endif
023000100115     C                   endif
023100100126     C                   endif
023200100126     C*
023300100126     C* Verifica tipo servizio E/H <=> ESTERO
023400100126     C                   if        i95tsp =  'E' OR
023500100126     C                             i95tsp =  'H'
023600100126     C                   if        i95nar <> *blanks OR
023700100126     C                             i95fi1 <> *blanks
023800110801     C                   eval      o95err = 'S'
023900100126     C                   eval      o95msg = MSGORI(24)
024000100126     C                   endif
024100100126     C                   endif
024200130701     C*
024300130701     C* Verifica tipo servizio H <=> Destinatario Supermercato
024400130701     C                   if        i95tsp =  'H' and i95fi2 = ' S'
024500130701     C                   eval      o95err = 'S'
024600130701     C                   eval      o95msg = MSGORI(24)
024700130701     C                   endif
024800100331     C*
024900100331     C                   endif
025000100416     C                   endif
025100090810     C*
025200090810     C                   ENDSR
025300090810
025400090810
025500090810      /TITLE Valorizzazione finale parametri d output
025600090810     C     valPAROUT     BEGSR
025700090810     C*
025800091027     C* Applico localizzazione messaggi
025900091007     C                   eval      jMsg = 1
026000091007     C*
026100091027     C     o95msg        lookup    MSGORI(jMsg)                           30
026200091007     C                   if        *in30
026300091027     C                   eval      tisi95r1ds.si95ojmsg = (jMsg)
026400110801     C                   eval      tisi95r1ds.si95ocmsg = %subst(o95msg:78:1) +
026500110801     C                                                    '*'
026600100407     C                   if        o95err = *blanks
026700100407     C                   eval      o95err = tisi95r1ds.si95ocmsg
026800100407     C                   endif
026900110801     C*
027000110801     C* Decodifiche
027100091007     C                   select
027200091027     C* ... Italiano
027300091027     C                   when      tisi95r1ds.si95ilang        = 'ITA' OR
027400091027     C                             %trim(tisi95r1ds.si95ilang) = 'IT'
027500091027     C                   eval      tisi95r1ds.si95odmsg = MSGITA(jMsg)
027600091027     C* ... Inglese
027700091027     C                   when      tisi95r1ds.si95ilang        = 'ENG' OR
027800091027     C                             %trim(tisi95r1ds.si95ilang) = 'EN'
027900091027     C                   eval      tisi95r1ds.si95odmsg = MSGENG(jMsg)
028000091027     C* ... Francese
028100091027     C                   when      tisi95r1ds.si95ilang        = 'FRA' OR
028200091027     C                             %trim(tisi95r1ds.si95ilang) = 'FR'
028300091027     C                   eval      tisi95r1ds.si95odmsg = MSGFRA(jMsg)
028400091027     C* ... Tedesco
028500091027     C                   when      tisi95r1ds.si95ilang        = 'DEU' OR
028600091027     C                             %trim(tisi95r1ds.si95ilang) = 'DE'
028700091027     C                   eval      tisi95r1ds.si95odmsg = MSGDEU(jMsg)
028800090810     C                   other
028900090810     C                   endsl
029000091007     C                   endif
029100090810     C*
029200091027     C                   eval      tisi95r1ds.tisi95ds = tisi95ds
029300090810     C*
029400090810     C                   ENDSR
029500091027** MSGORI - MESSAGGI
029600091027TISI95R- Nazione inesistente.                                                1
029700091027TISI95R- CAP formalmente errato.                                             2
029800091027TISI95R- Richiesti tempi di consegna senza inserire il tipo servizio.        3
029900091027TISI95R- Richiesto controllo eccezioni senza AS di partenza.                 4
030000091027TISI95R- 5                                                                   5
030100091027TISI95R- 6                                                                   6
030200091027TISI95R- 7                                                                   7
030300091027TISI95R- CAP inesistente.                                                    8
030400091027TISI95R- Provincia diversa dalla provincia della località.                   9
030500100504TISI95R- Tipo controllo errato.                                              A                    10
030600100504TISI95R- Località inesistente.                                               B                    11
030700100504TISI95R- Provincia inesistente.                                              C                    12
030800100504TISI95R- Località inesistente in questo CAP.                                 D                    13
030900100504TISI95R- Provincia diversa da quella del CAP.                                E                    14
031000100504TISI95R- Tipo tempo/tipo servizio non trovati.                               F                    15
031100100504TISI95R- Versione cappario non trovata.                                      G                    16
031200100504TISI95R- Località esistente ma non univoca.                                  H                    17
031300100504TISI95R- Località obbligatoria.                                              I                    18
031400100504TISI95R- 19                                                                  L                    19
031500100504TISI95R- Errore generico.                                                    M                    20
031600100504TISI57R- Tipo servizio H non disponibile per la destinazione indicata.       P                    21
031700100504TISI57R- Tipo servizio non disponibile per peso/volume/colli indicati.       Q                    22
031800100504TISI57R- Network non disponibile per peso/volume/colli indicati.             R                    23
031900100504TISI57R- Tipo servizio non disponibile per nazione/network indicati.         S                    24
032000100504TISI57R- Network non disponibile per nazione di destinazione indicata.       T                    25
032100100504TISI97R- Impossibile spedire per la destinazione indicata.                   U                    26
032200100504TISI97R- Spedizioni in contrassegno non consentite per la destinazione.      V                    27
032300100504TISI97R- Spedizioni in porto assegnato non consentite per la destinazione.   W                    28
032400100504TISI97R- Caratteristiche della spedizone incompatibili con la destinazione.  X                    29
032500100504                                                                             Y                    30
032600090810** MSGITA - MESSAGGI
032700091027Nazione inesistente.
032800091027CAP formalmente errato.
032900091027Dati forniti insufficienti, indicare il tipo servizio spedizione.
033000091027Dati forniti insufficienti, indicare il terminal di partenza.
0331000910275
0332000910276
0333000910277
033400091027CAP inesistente.
033500091027Provincia diversa dalla provincia della località.
03360009102710
033700091027Località inesistente.
033800091027Provincia inesistente.
033900091027Località inesistente in questo CAP.
034000091027Provincia incongruente rispetto al CAP.
034100091027Tipo servizio spedizione inesistente.
034200091027Data di riferimento non valida.
034300091027Località esistente ma non univoca.
034400091027Località obbligatoria.
03450009102719
034600091027Errore generico.
034700110524SERVIZIO 10:30 non disponibile per la destinazione indicata.
034800100115Servizio non disponibile per peso/volume/colli indicati.
034900100114Network non disponibile per peso/volume/colli indicati.
035000100114Tipo servizio non disponibile per nazione/network indicati.
035100100115Network non disponibile per nazione di destinazione indicata.
035200100504Impossibile spedire per la destinazione indicata.
035300100504Spedizioni in contrassegno non consentite per la destinazione.
035400100504Spedizioni in porto assegnato non consentite per la destinazione.
035500100504Caratteristiche della spedizone incompatibili con la destinazione.
035600100504
035700091027** MSGENG - MESSAGGI
035800091030Nonexistent country
035900091030Wrong ZIP code
036000091030The given data is insufficient, please give the kind of shipment service
036100091030The given data is insufficient, please give the departing hub
036200091030
036300091030
036400091030
036500091030Nonexistent ZIP code
036600091030The district is different from the town district
036700091030
036800091030Nonexistent place
036900091030Nonexistent district
037000091030The place is nonexistent at this ZIP code
037100091030The district does not match the ZIP code
037200091030Nonexistent kind of shipment service
037300091030Not effective reference date
037400091030The place exists but we need more details about it
037500091030Compulsory place
037600091030
037700091030Generic error
037800110524SERVIZIO 10.30 service is not available for the chosen place.
037900100115This kind of service is not available for the chosen weight/volume/number of parcels
038000100115The network is not available for the chosen weight/volume/number of parcels
038100100115This kind of service is not available for the chosen country/network
038200100115The network is not available for the chosen country
038300100504Unable to ship to chosen Network/destination
038400100504Shipments with C.O.D. not allow for the chosen network/destination
038500100504EX WORKS shipments not allow for the chosen network/destination
038600100504Some features of shipment are incompatible with chosen network/destination
038700100504
038800091027** MSGFRA - MESSAGGI
038900091030Pays inexistant
039000091030Code postal erroné
039100091030Donnés insuffisants, donner le type de services de l'expédition
039200091030Les donnés sont insuffisants, donner l'hub de départ
039300091030
039400091030
039500091030
039600091030Le code postal n'existe pas
039700091030Province différente par rapport à la province de la localité
039800091030
039900091030Localité inexistante
040000091030Province inexistante
040100091030Localité inexistante à ce code postal
040200091030La province ne corrésponds pas à ce code postal
040300091030Le type de service de l'expédition n'existe pas
040400091030La date de référence n'est pas valable
040500091030Localité existante mais nous avons besoin de plus de détails
040600091030La localité est obligatoire
040700091028
040800091028Erreur générique
040900110524Le service SERVIZIO 10:30 n'est pas disponible pour le destin indiqué
041000100115Ce type de service n'est pas disponible pour le poids/volume/colis indiqués
041100100115Ce réseau n'est pas disponible pour le poids/volume/colis indiqués
041200100115Ce service n'est pas disponible pour la nation/réseau indiqués
041300100115Ce réseau n'est pas disponible pour le nation indiqués
041400100504Impossible d'envoyer au réseau/destin indiqué
041500100504Livraisons avec paiement n'est pas disponible pour le réseau/destin indiqué
041600100504EX WORKS expéditions permettent pas pour le réseau/destin indiqué
041700100504Expéditions avec fonctionnalités incompatibles avec le réseau/destin indiqué
041800100504
041900091027** MSGDEU - MESSAGGI
042000091030Inexistenter Staat
042100091030Falsches Postfach
042200091030Die Daten sind ungenügend, bitte geben der Typ des Sendungdiensts
042300091030Die Daten sind ungenügend, bitte geben das Abfahrtlager
042400091030
042500091030
042600091030
042700091030Inexistentes Postfach
042800091030Die Provinz ist anders als die Provinz des Orts
042900091030
043000091030Inexistenter Ort
043100091030Inexistente Provinz
043200091030Inexistenter Ort mit diesem Postfach
043300091030Das Postfach korrespondiert nicht mit der Provinz
043400091030Inexistenter Sendungdienst
043500091030Das Datum ist nicht gültig
043600091030Der Name des Orts ist nicht detailiert genug
043700091030Der Ort ist obligatorisch
043800091030
043900091028Allgemeiner Fehler
044000110524SERVIZIO 10:30 Uhr Service fuer folgende Bestimmungsorte nicht vorhanden
044100100504Service fuer die angegebenen Gewichte/Volumen/Kolli nicht vorhanden
044200100115Network fuer die angegebenen Gewichte/Volumen/Kolli nicht vorhanden
044300100504Service fuer die angegebenen Laender/Network nicht vorhanden
044400100115Network fuer die angegebenen Laender nicht vorhanden
044500100504Keine Verbindung zum Network/Bestimmungsorte gewählt senden
044600100504Sendungen mit C.O.D. für die angegebenen Network/Bestimmungsorte nicht vorhanden
044700100504EX WORKS Sendungen für die angegebenen Network/Bestimmungsorte nicht vorhanden
044800100504Features Expeditionen fuer dem Network/Bestimmungsorte nicht vorhanden
044900100504
