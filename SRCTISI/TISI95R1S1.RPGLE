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
024200100331     C*
024300100331     C                   endif
024400100416     C                   endif
024500090810     C*
024600090810     C                   ENDSR
024700090810
024800090810
024900090810      /TITLE Valorizzazione finale parametri d output
025000090810     C     valPAROUT     BEGSR
025100090810     C*
025200091027     C* Applico localizzazione messaggi
025300091007     C                   eval      jMsg = 1
025400091007     C*
025500091027     C     o95msg        lookup    MSGORI(jMsg)                           30
025600091007     C                   if        *in30
025700091027     C                   eval      tisi95r1ds.si95ojmsg = (jMsg)
025800110801     C                   eval      tisi95r1ds.si95ocmsg = %subst(o95msg:78:1) +
025900110801     C                                                    '*'
026000100407     C                   if        o95err = *blanks
026100100407     C                   eval      o95err = tisi95r1ds.si95ocmsg
026200100407     C                   endif
026300110801     C*
026400110801     C* Decodifiche
026500091007     C                   select
026600091027     C* ... Italiano
026700091027     C                   when      tisi95r1ds.si95ilang        = 'ITA' OR
026800091027     C                             %trim(tisi95r1ds.si95ilang) = 'IT'
026900091027     C                   eval      tisi95r1ds.si95odmsg = MSGITA(jMsg)
027000091027     C* ... Inglese
027100091027     C                   when      tisi95r1ds.si95ilang        = 'ENG' OR
027200091027     C                             %trim(tisi95r1ds.si95ilang) = 'EN'
027300091027     C                   eval      tisi95r1ds.si95odmsg = MSGENG(jMsg)
027400091027     C* ... Francese
027500091027     C                   when      tisi95r1ds.si95ilang        = 'FRA' OR
027600091027     C                             %trim(tisi95r1ds.si95ilang) = 'FR'
027700091027     C                   eval      tisi95r1ds.si95odmsg = MSGFRA(jMsg)
027800091027     C* ... Tedesco
027900091027     C                   when      tisi95r1ds.si95ilang        = 'DEU' OR
028000091027     C                             %trim(tisi95r1ds.si95ilang) = 'DE'
028100091027     C                   eval      tisi95r1ds.si95odmsg = MSGDEU(jMsg)
028200090810     C                   other
028300090810     C                   endsl
028400091007     C                   endif
028500090810     C*
028600091027     C                   eval      tisi95r1ds.tisi95ds = tisi95ds
028700090810     C*
028800090810     C                   ENDSR
028900091027** MSGORI - MESSAGGI
029000091027TISI95R- Nazione inesistente.                                                1
029100091027TISI95R- CAP formalmente errato.                                             2
029200091027TISI95R- Richiesti tempi di consegna senza inserire il tipo servizio.        3
029300091027TISI95R- Richiesto controllo eccezioni senza AS di partenza.                 4
029400091027TISI95R- 5                                                                   5
029500091027TISI95R- 6                                                                   6
029600091027TISI95R- 7                                                                   7
029700091027TISI95R- CAP inesistente.                                                    8
029800091027TISI95R- Provincia diversa dalla provincia della località.                   9
029900100504TISI95R- Tipo controllo errato.                                              A                    10
030000100504TISI95R- Località inesistente.                                               B                    11
030100100504TISI95R- Provincia inesistente.                                              C                    12
030200100504TISI95R- Località inesistente in questo CAP.                                 D                    13
030300100504TISI95R- Provincia diversa da quella del CAP.                                E                    14
030400100504TISI95R- Tipo tempo/tipo servizio non trovati.                               F                    15
030500100504TISI95R- Versione cappario non trovata.                                      G                    16
030600100504TISI95R- Località esistente ma non univoca.                                  H                    17
030700100504TISI95R- Località obbligatoria.                                              I                    18
030800100504TISI95R- 19                                                                  L                    19
030900100504TISI95R- Errore generico.                                                    M                    20
031000100504TISI57R- Tipo servizio H non disponibile per la destinazione indicata.       P                    21
031100100504TISI57R- Tipo servizio non disponibile per peso/volume/colli indicati.       Q                    22
031200100504TISI57R- Network non disponibile per peso/volume/colli indicati.             R                    23
031300100504TISI57R- Tipo servizio non disponibile per nazione/network indicati.         S                    24
031400100504TISI57R- Network non disponibile per nazione di destinazione indicata.       T                    25
031500100504TISI97R- Impossibile spedire per la destinazione indicata.                   U                    26
031600100504TISI97R- Spedizioni in contrassegno non consentite per la destinazione.      V                    27
031700100504TISI97R- Spedizioni in porto assegnato non consentite per la destinazione.   W                    28
031800100504TISI97R- Caratteristiche della spedizone incompatibili con la destinazione.  X                    29
031900100504                                                                             Y                    30
032000090810** MSGITA - MESSAGGI
032100091027Nazione inesistente.
032200091027CAP formalmente errato.
032300091027Dati forniti insufficienti, indicare il tipo servizio spedizione.
032400091027Dati forniti insufficienti, indicare il terminal di partenza.
0325000910275
0326000910276
0327000910277
032800091027CAP inesistente.
032900091027Provincia diversa dalla provincia della località.
03300009102710
033100091027Località inesistente.
033200091027Provincia inesistente.
033300091027Località inesistente in questo CAP.
033400091027Provincia incongruente rispetto al CAP.
033500091027Tipo servizio spedizione inesistente.
033600091027Data di riferimento non valida.
033700091027Località esistente ma non univoca.
033800091027Località obbligatoria.
03390009102719
034000091027Errore generico.
034100110524SERVIZIO 10:30 non disponibile per la destinazione indicata.
034200100115Servizio non disponibile per peso/volume/colli indicati.
034300100114Network non disponibile per peso/volume/colli indicati.
034400100114Tipo servizio non disponibile per nazione/network indicati.
034500100115Network non disponibile per nazione di destinazione indicata.
034600100504Impossibile spedire per la destinazione indicata.
034700100504Spedizioni in contrassegno non consentite per la destinazione.
034800100504Spedizioni in porto assegnato non consentite per la destinazione.
034900100504Caratteristiche della spedizone incompatibili con la destinazione.
035000100504
035100091027** MSGENG - MESSAGGI
035200091030Nonexistent country
035300091030Wrong ZIP code
035400091030The given data is insufficient, please give the kind of shipment service
035500091030The given data is insufficient, please give the departing hub
035600091030
035700091030
035800091030
035900091030Nonexistent ZIP code
036000091030The district is different from the town district
036100091030
036200091030Nonexistent place
036300091030Nonexistent district
036400091030The place is nonexistent at this ZIP code
036500091030The district does not match the ZIP code
036600091030Nonexistent kind of shipment service
036700091030Not effective reference date
036800091030The place exists but we need more details about it
036900091030Compulsory place
037000091030
037100091030Generic error
037200110524SERVIZIO 10.30 service is not available for the chosen place.
037300100115This kind of service is not available for the chosen weight/volume/number of parcels
037400100115The network is not available for the chosen weight/volume/number of parcels
037500100115This kind of service is not available for the chosen country/network
037600100115The network is not available for the chosen country
037700100504Unable to ship to chosen Network/destination
037800100504Shipments with C.O.D. not allow for the chosen network/destination
037900100504EX WORKS shipments not allow for the chosen network/destination
038000100504Some features of shipment are incompatible with chosen network/destination
038100100504
038200091027** MSGFRA - MESSAGGI
038300091030Pays inexistant
038400091030Code postal erroné
038500091030Donnés insuffisants, donner le type de services de l'expédition
038600091030Les donnés sont insuffisants, donner l'hub de départ
038700091030
038800091030
038900091030
039000091030Le code postal n'existe pas
039100091030Province différente par rapport à la province de la localité
039200091030
039300091030Localité inexistante
039400091030Province inexistante
039500091030Localité inexistante à ce code postal
039600091030La province ne corrésponds pas à ce code postal
039700091030Le type de service de l'expédition n'existe pas
039800091030La date de référence n'est pas valable
039900091030Localité existante mais nous avons besoin de plus de détails
040000091030La localité est obligatoire
040100091028
040200091028Erreur générique
040300110524Le service SERVIZIO 10:30 n'est pas disponible pour le destin indiqué
040400100115Ce type de service n'est pas disponible pour le poids/volume/colis indiqués
040500100115Ce réseau n'est pas disponible pour le poids/volume/colis indiqués
040600100115Ce service n'est pas disponible pour la nation/réseau indiqués
040700100115Ce réseau n'est pas disponible pour le nation indiqués
040800100504Impossible d'envoyer au réseau/destin indiqué
040900100504Livraisons avec paiement n'est pas disponible pour le réseau/destin indiqué
041000100504EX WORKS expéditions permettent pas pour le réseau/destin indiqué
041100100504Expéditions avec fonctionnalités incompatibles avec le réseau/destin indiqué
041200100504
041300091027** MSGDEU - MESSAGGI
041400091030Inexistenter Staat
041500091030Falsches Postfach
041600091030Die Daten sind ungenügend, bitte geben der Typ des Sendungdiensts
041700091030Die Daten sind ungenügend, bitte geben das Abfahrtlager
041800091030
041900091030
042000091030
042100091030Inexistentes Postfach
042200091030Die Provinz ist anders als die Provinz des Orts
042300091030
042400091030Inexistenter Ort
042500091030Inexistente Provinz
042600091030Inexistenter Ort mit diesem Postfach
042700091030Das Postfach korrespondiert nicht mit der Provinz
042800091030Inexistenter Sendungdienst
042900091030Das Datum ist nicht gültig
043000091030Der Name des Orts ist nicht detailiert genug
043100091030Der Ort ist obligatorisch
043200091030
043300091028Allgemeiner Fehler
043400110524SERVIZIO 10:30 Uhr Service fuer folgende Bestimmungsorte nicht vorhanden
043500100504Service fuer die angegebenen Gewichte/Volumen/Kolli nicht vorhanden
043600100115Network fuer die angegebenen Gewichte/Volumen/Kolli nicht vorhanden
043700100504Service fuer die angegebenen Laender/Network nicht vorhanden
043800100115Network fuer die angegebenen Laender nicht vorhanden
043900100504Keine Verbindung zum Network/Bestimmungsorte gewählt senden
044000100504Sendungen mit C.O.D. für die angegebenen Network/Bestimmungsorte nicht vorhanden
044100100504EX WORKS Sendungen für die angegebenen Network/Bestimmungsorte nicht vorhanden
044200100504Features Expeditionen fuer dem Network/Bestimmungsorte nicht vorhanden
044300100504
