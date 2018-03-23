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
003600161104     C*
003700161104     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
003800161104     C
003900161104     C/EXEC SQL
004000161104     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
004100161104     C/END-EXEC
004200161104     C
004300100114     C*
004400100114     C* Chiave su TABEL00F - Completa
004500100114     C     KEYtab_C      KLIST
004600100114     C                   KFLD                    tblKUT
004700100114     C                   KFLD                    tblCOD
004800100114     C                   KFLD                    tblKEY
004900091027     C*
005000091027     C* Verifica parametri di input
005100091027     C                   exsr      chkPARIN
005200090810     C*
005300090810     C* Calcolo instradamento
005400090810     C                   exsr      calINSTR
005500090810     C*
005600090810     C* Valorizzazione finale parametri d output
005700090810     C                   exsr      valPAROUT
005800090810     C*
005900090810     C                   return
006000091027
006100091027
006200091027      /TITLE Verifica parametri di input
006300091027     C     chkPARIN      BEGSR
006400091027     C*
006500091027     C                   if        tisi95r1ds.si95ilang = *blanks
006600091027     C                   eval      tisi95r1ds.si95ilang = 'IT'
006700091027     C                   else
006800091027     C                   eval      tisi95r1ds.si95ilang =
006900091027     C                                   %xlate(minu:maiu:tisi95r1ds.si95ilang)
007000091027     C                   endif
007100091027     C*
007200091027     C                   ENDSR
007300090810
007400090810
007500090810      /TITLE Calcolo instradamento
007600090810     C     calINSTR      BEGSR
007700100805     C*
007800100805     C* Inizializzazioni
007900100805     C                   clear                   dcppflo
008000100805     C                   clear                   tisi97ds
008100091027     C*
008200100805     C* passagggio parametri
008300091027     C                   eval      tisi95ds = tisi95r1ds.tisi95ds
008400100416     C*
008500100416     C* <<< CONTROLLI PRIMARI >>>
008600100416     C*
008700100416     C* >>> CONTROLLI PRIMARI <<<
008800091027     C*
008900100416     C* Procedo solo se nn presenti errori nei controlli PRIMARI
009000100416     C                   if        o95msg = *blanks
009100100416     C*
009200091027     C                   call      'TISI95R'
009300091027     C                   parm                    tisi95ds
009400100331     C*
009500100331     C* I controlli seguenti sono subordinati alla chiamata x "calcolo instradamento"
009600100331     C                   if        tisi95r1ds.si95itype = 'C'
009700100507     C*
009800100507     C* Verifiche Italia <=> network estero
009900100507     C                   if        o95err = *blanks
010000100507     C                   if        i95nar  = *blanks AND
010100100507     C                             i95fi1 <> *blanks
010200110801     C                   eval      o95err = 'T'
010300100507     C                   eval      o95msg = MSGORI(25)
010400100507     C                   endif
010500100507     C                   endif
010600100507     C*
010700100507     C* Verifiche estero <=> network Italia
010800100507     C                   if        o95err = *blanks
010900100507     C                   if        i95nar <> *blanks AND
011000100507     C                             i95fi1  = *blanks
011100110801     C                   eval      o95err = 'T'
011200100507     C                   eval      o95msg = MSGORI(25)
011300100507     C                   endif
011400100507     C                   endif
011500100507     C*
011600100507     C* Verifiche estero <=> network esteri
011700100507     C                   if        o95msg = *blanks
011800100507     C                   if        i95nar <> *blanks AND
011900100507     C                             i95fi1 <> *blanks
012000100507     C                   eval      tblKUT = 1
012100100507     C                   eval      tblCOD = '15'
012200100507     C                   eval      tblKEY = i95nar
012300100507     C     KEYtab_C      chain     tabel00f
012400100507     C                   if        %found(tabel00f)
012500100507     C                   eval      ds15 = tblUNI
012600100507     C                   select
012700100507     C                   when      i95fi1 = 'E' and §15EEX <> 'S'
012800110801     C                   eval      o95err = 'T'
012900100507     C                   eval      o95msg = MSGORI(25)
013000100507     C                   when      i95fi1 = 'D' and §15LAD  = *zeros
013100110801     C                   eval      o95err = 'T'
013200100507     C                   eval      o95msg = MSGORI(25)
013300100507     C                   when      i95fi1 = 'S' and §15LAF  = *zeros
013400110801     C                   eval      o95err = 'T'
013500100507     C                   eval      o95msg = MSGORI(25)
013600100507     C                   endsl
013700100507     C                   endif
013800100507     C                   endif
013900100507     C                   endif
014000100504     C*
014100100504     C* Verifico Utilizzi Particolari CAP
014200100504     C                   if        o95err = *blanks
014300100504     C                   select
014400100504     C                   when      i95fi1 = 'E'
014500100504     C                   eval      i97ntw = 'EEX'
014600100504     C                   when      i95fi1 = 'D'
014700100504     C                   eval      i97ntw = 'DPD'
014800100504     C                   when      i95fi1 = 'S'
014900100504     C                   eval      i97ntw = 'FED'
015000100504     C                   endsl
015100100504     C                   call      'TISI97R'
015200100504     C                   parm                    tisi95ds
015300100504     C                   parm                    tiSI97ds
015400100504     C                   if        o97err = ' ' AND o97uti = 'N'
015500100504     C                   eval      dcppflo = o97flo
015600100504     C                   select
015700100504     C* No tutte spedizioni...
015800100504     C                   when      §cppall='S'
015900110801     C                   eval      o95err = 'U'
016000100504     C                   eval      o95msg = MSGORI(26)
016100100504     C* No spedizioni in contrassegno...
016200100504     C                   when      §cppcas = 'S' and i95fca = 'S'
016300110801     C                   eval      o95err = 'V'
016400100504     C                   eval      o95msg = MSGORI(27)
016500100504     C* No spedizioni in porto assegnato...
016600100504     C                   when      §cpppas <> ' ' and i95tpo = 'A'
016700110801     C                   eval      o95err = 'W'
016800100504     C                   eval      o95msg = MSGORI(28)
016900100504     C                   other
017000100504     C* No spedizioni "altro"...
017100110801     C***                eval      o95err = 'X'
017200100504     C***                eval      o95msg = MSGORI(29)
017300100504     C                   endsl
017400100504     C                   endif
017500100504     C                   endif
017600100114     C*
017700100114     C* Verifiche specifiche x tipo servizio "H"
017800100114     C                   if        o95err = *blanks
017900100114     C                   if        i95tsp = 'H' and
018000100114     C                             o95ttc < 1000
018100110801     C                   eval      o95err = 'P'
018200100114     C                   eval      o95msg = MSGORI(21)
018300100114     C                   endif
018400161104     C                   endif
018500100114     C*
018600100114     C* Verifica parametri "TISI57" - Limiti x tipo servizio
018700100114     C                   if        o95msg = *blanks
018800100114     C                   eval      tblKUT = 1
018900100114     C                   eval      tblCOD = 'T5'
019000100114     C                   eval      tblKEY = i95tsp
019100100114     C     KEYtab_C      chain     tabel00f
019200100114     C                   if        %found(tabel00f)
019300100114     C                   eval      dsT5 = tblUNI
019400100114     C                   if        tisi95r1ds.si95incl > §T5NCL OR
019500100114     C                             i95lkg              > §T5PKG OR
019600100114     C                             i95lmc              > §T5VLM
019700110801     C                   eval      o95err = 'Q'
019800100114     C                   eval      o95msg = MSGORI(22)
019900100114     C                   endif
020000100114     C                   endif
020100100114     C                   endif
020200100114     C*
020300100114     C* Verifica parametri "TISI57" - Limiti x network
020400100114     C                   if        o95msg = *blanks
020500100114     C                   eval      tblKEY = *blanks
020600100504     C                   select
020700100504     C                   when      i95fi1 = 'E'
020800100504     C                   eval      tblKEY = 'EEX'
020900100504     C                   when      i95fi1 = 'D'
021000100114     C                   eval      tblKEY = 'DPD'
021100100504     C                   when      i95fi1 = 'S'
021200100114     C                   eval      tblKEY = 'FED'
021300100504     C                   endsl
021400100114     C                   eval      tblKUT = 1
021500100114     C                   eval      tblCOD = 'T5'
021600100114     C     KEYtab_C      chain     tabel00f
021700100114     C                   if        %found(tabel00f)
021800100114     C                   eval      dsT5 = tblUNI
021900100114     C                   if        tisi95r1ds.si95incl > §T5NCL OR
022000100114     C                             i95lkg              > §T5PKG OR
022100100114     C                             i95lmc              > §T5VLM
022200110801     C                   eval      o95err = 'R'
022300100114     C                   eval      o95msg = MSGORI(23)
022400100114     C                   endif
022500100114     C                   endif
022600100114     C                   endif
022700100115     C*
022800100115     C* Verifiche network <=> estero
022900100126     C                   if        o95msg = *blanks
023000100115     C                   if        o95lna =  *zeros AND
023100100115     C                             o95znc =  *zeros
023200100115     C                   if        i95nar <> *blanks OR
023300100115     C                             i95fi1 <> *blanks
023400110801     C                   eval      o95err = 'T'
023500100115     C                   eval      o95msg = MSGORI(25)
023600100115     C                   endif
023700100115     C                   endif
023800100126     C                   endif
023900100126     C*
024000100126     C* Verifica tipo servizio E/H <=> ESTERO
024100100126     C                   if        i95tsp =  'E' OR
024200100126     C                             i95tsp =  'H'
024300100126     C                   if        i95nar <> *blanks OR
024400100126     C                             i95fi1 <> *blanks
024500110801     C                   eval      o95err = 'S'
024600100126     C                   eval      o95msg = MSGORI(24)
024700100126     C                   endif
024800100126     C                   endif
024900130701     C*
025000130701     C* Verifica tipo servizio H <=> Destinatario Supermercato
025100130701     C                   if        i95tsp =  'H' and i95fi2 = ' S'
025200130701     C                   eval      o95err = 'S'
025300130701     C                   eval      o95msg = MSGORI(24)
025400130701     C                   endif
025500161104     C*
025600161104     C* Verifiche specifiche x tipo servizio H/E in Italia
025700161104     C                   if        o95err = *blanks
025800161104     C                   if        i95nar = *blanks AND
025900161104     C                             i95tsp = 'H'
026000161104     C                   z-add     *zeros        TempiResaCli      3 0
026100161104     C*
026200161104     C/EXEC SQL
026300161104     C+ select dlytrc into :TempiResaCli from wadly00f
026400161104     C+ where dlylnp = :i95tfp  AND
026500161104     C+       dlytsp = :i95tsp  AND
026600161104     C+       dlynar = :i95nar  AND
026700161104     C+       dlycap = :o95cap  AND
026800161104     C+       dlyloc = :o95Loc
026900161104     C/END-EXEC
027000161104     C*
027100161104     C                   if        sqlcod < *zeros or sqlcod=100 or
027200161104     C                             not (TempiResaCli = 10 or TempiResaCli = 34)
027300161104     C                   eval      o95err = 'P'
027400161104     C                   eval      o95msg = MSGORI(21)
027500161104     C                   endif
027600161104     C                   endif
027700161104     C                   endif
027800100331     C*
027900100331     C                   endif
028000100416     C                   endif
028100090810     C*
028200090810     C                   ENDSR
028300090810
028400090810
028500090810      /TITLE Valorizzazione finale parametri d output
028600090810     C     valPAROUT     BEGSR
028700090810     C*
028800091027     C* Applico localizzazione messaggi
028900091007     C                   eval      jMsg = 1
029000091007     C*
029100091027     C     o95msg        lookup    MSGORI(jMsg)                           30
029200091007     C                   if        *in30
029300091027     C                   eval      tisi95r1ds.si95ojmsg = (jMsg)
029400110801     C                   eval      tisi95r1ds.si95ocmsg = %subst(o95msg:78:1) +
029500110801     C                                                    '*'
029600100407     C                   if        o95err = *blanks
029700100407     C                   eval      o95err = tisi95r1ds.si95ocmsg
029800100407     C                   endif
029900110801     C*
030000110801     C* Decodifiche
030100091007     C                   select
030200091027     C* ... Italiano
030300091027     C                   when      tisi95r1ds.si95ilang        = 'ITA' OR
030400091027     C                             %trim(tisi95r1ds.si95ilang) = 'IT'
030500091027     C                   eval      tisi95r1ds.si95odmsg = MSGITA(jMsg)
030600091027     C* ... Inglese
030700091027     C                   when      tisi95r1ds.si95ilang        = 'ENG' OR
030800091027     C                             %trim(tisi95r1ds.si95ilang) = 'EN'
030900091027     C                   eval      tisi95r1ds.si95odmsg = MSGENG(jMsg)
031000091027     C* ... Francese
031100091027     C                   when      tisi95r1ds.si95ilang        = 'FRA' OR
031200091027     C                             %trim(tisi95r1ds.si95ilang) = 'FR'
031300091027     C                   eval      tisi95r1ds.si95odmsg = MSGFRA(jMsg)
031400091027     C* ... Tedesco
031500091027     C                   when      tisi95r1ds.si95ilang        = 'DEU' OR
031600091027     C                             %trim(tisi95r1ds.si95ilang) = 'DE'
031700091027     C                   eval      tisi95r1ds.si95odmsg = MSGDEU(jMsg)
031800090810     C                   other
031900090810     C                   endsl
032000091007     C                   endif
032100090810     C*
032200091027     C                   eval      tisi95r1ds.tisi95ds = tisi95ds
032300090810     C*
032400090810     C                   ENDSR
032500161104** MSGORI - MESSAGGI
032600161104TISI95R- Nazione inesistente.                                                1
032700161104TISI95R- CAP formalmente errato.                                             2
032800161104TISI95R- Richiesti tempi di consegna senza inserire il tipo servizio.        3
032900161104TISI95R- Richiesto controllo eccezioni senza AS di partenza.                 4
033000161104TISI95R- 5                                                                   5
033100161104TISI95R- 6                                                                   6
033200161104TISI95R- 7                                                                   7
033300161104TISI95R- CAP inesistente.                                                    8
033400161104TISI95R- Provincia diversa dalla provincia della località.                   9
033500161104TISI95R- Tipo controllo errato.                                              A                    10
033600161104TISI95R- Località inesistente.                                               B                    11
033700161104TISI95R- Provincia inesistente.                                              C                    12
033800161104TISI95R- Località inesistente in questo CAP.                                 D                    13
033900161104TISI95R- Provincia diversa da quella del CAP.                                E                    14
034000161104TISI95R- Tipo tempo/tipo servizio non trovati.                               F                    15
034100161104TISI95R- Versione cappario non trovata.                                      G                    16
034200161104TISI95R- Località esistente ma non univoca.                                  H                    17
034300161104TISI95R- Località obbligatoria.                                              I                    18
034400161104TISI95R- 19                                                                  L                    19
034500161104TISI95R- Errore generico.                                                    M                    20
034600161104TISI57R- Tipo servizio H non disponibile per la destinazione indicata.       P                    21
034700161104TISI57R- Tipo servizio non disponibile per peso/volume/colli indicati.       Q                    22
034800161104TISI57R- Network non disponibile per peso/volume/colli indicati.             R                    23
034900161104TISI57R- Tipo servizio non disponibile per nazione/network indicati.         S                    24
035000161104TISI57R- Network non disponibile per nazione di destinazione indicata.       T                    25
035100161104TISI97R- Impossibile spedire per la destinazione indicata.                   U                    26
035200161104TISI97R- Spedizioni in contrassegno non consentite per la destinazione.      V                    27
035300161104TISI97R- Spedizioni in porto assegnato non consentite per la destinazione.   W                    28
035400161104TISI97R- Caratteristiche della spedizone incompatibili con la destinazione.  X                    29
035500161104                                                                             Y                    30
035600161104** MSGITA - MESSAGGI
035700161104Nazione inesistente.
035800161104CAP formalmente errato.
035900161104Dati forniti insufficienti, indicare il tipo servizio spedizione.
036000161104Dati forniti insufficienti, indicare il terminal di partenza.
0361001611045
0362001611046
0363001611047
036400161104CAP inesistente.
036500161104Provincia diversa dalla provincia della località.
03660016110410
036700161104Località inesistente.
036800161104Provincia inesistente.
036900161104Località inesistente in questo CAP.
037000161104Provincia incongruente rispetto al CAP.
037100161104Tipo servizio spedizione inesistente.
037200161104Data di riferimento non valida.
037300161104Località esistente ma non univoca.
037400161104Località obbligatoria.
03750016110419
037600161104Errore generico.
037700161104SERVIZIO 10:30 non disponibile per la destinazione indicata.
037800161104Servizio non disponibile per peso/volume/colli indicati.
037900161104Network non disponibile per peso/volume/colli indicati.
038000161104Tipo servizio non disponibile per nazione/network indicati.
038100161104Network non disponibile per nazione di destinazione indicata.
038200161104Impossibile spedire per la destinazione indicata.
038300161104Spedizioni in contrassegno non consentite per la destinazione.
038400161104Spedizioni in porto assegnato non consentite per la destinazione.
038500161104Caratteristiche della spedizone incompatibili con la destinazione.
038600161104
038700161104** MSGENG - MESSAGGI
038800161104Nonexistent country
038900161104Wrong ZIP code
039000161104The given data is insufficient, please give the kind of shipment service
039100161104The given data is insufficient, please give the departing hub
039200161104
039300161104
039400161104
039500161104Nonexistent ZIP code
039600161104The district is different from the town district
039700161104
039800161104Nonexistent place
039900161104Nonexistent district
040000161104The place is nonexistent at this ZIP code
040100161104The district does not match the ZIP code
040200161104Nonexistent kind of shipment service
040300161104Not effective reference date
040400161104The place exists but we need more details about it
040500161104Compulsory place
040600161104
040700161104Generic error
040800161104SERVIZIO 10.30 service is not available for the chosen place.
040900161104This kind of service is not available for the chosen weight/volume/number of parcels
041000161104The network is not available for the chosen weight/volume/number of parcels
041100161104This kind of service is not available for the chosen country/network
041200161104The network is not available for the chosen country
041300161104Unable to ship to chosen Network/destination
041400161104Shipments with C.O.D. not allow for the chosen network/destination
041500161104EX WORKS shipments not allow for the chosen network/destination
041600161104Some features of shipment are incompatible with chosen network/destination
041700161104
041800161104** MSGFRA - MESSAGGI
041900161104Pays inexistant
042000161104Code postal erroné
042100161104Donnés insuffisants, donner le type de services de l'expédition
042200161104Les donnés sont insuffisants, donner l'hub de départ
042300161104
042400161104
042500161104
042600161104Le code postal n'existe pas
042700161104Province différente par rapport à la province de la localité
042800161104
042900161104Localité inexistante
043000161104Province inexistante
043100161104Localité inexistante à ce code postal
043200161104La province ne corrésponds pas à ce code postal
043300161104Le type de service de l'expédition n'existe pas
043400161104La date de référence n'est pas valable
043500161104Localité existante mais nous avons besoin de plus de détails
043600161104La localité est obligatoire
043700161104
043800161104Erreur générique
043900161104Le service SERVIZIO 10:30 n'est pas disponible pour le destin indiqué
044000161104Ce type de service n'est pas disponible pour le poids/volume/colis indiqués
044100161104Ce réseau n'est pas disponible pour le poids/volume/colis indiqués
044200161104Ce service n'est pas disponible pour la nation/réseau indiqués
044300161104Ce réseau n'est pas disponible pour le nation indiqués
044400161104Impossible d'envoyer au réseau/destin indiqué
044500161104Livraisons avec paiement n'est pas disponible pour le réseau/destin indiqué
044600161104EX WORKS expéditions permettent pas pour le réseau/destin indiqué
044700161104Expéditions avec fonctionnalités incompatibles avec le réseau/destin indiqué
044800161104
044900161104** MSGDEU - MESSAGGI
045000161104Inexistenter Staat
045100161104Falsches Postfach
045200161104Die Daten sind ungenügend, bitte geben der Typ des Sendungdiensts
045300161104Die Daten sind ungenügend, bitte geben das Abfahrtlager
045400161104
045500161104
045600161104
045700161104Inexistentes Postfach
045800161104Die Provinz ist anders als die Provinz des Orts
045900161104
046000161104Inexistenter Ort
046100161104Inexistente Provinz
046200161104Inexistenter Ort mit diesem Postfach
046300161104Das Postfach korrespondiert nicht mit der Provinz
046400161104Inexistenter Sendungdienst
046500161104Das Datum ist nicht gültig
046600161104Der Name des Orts ist nicht detailiert genug
046700161104Der Ort ist obligatorisch
046800161104
046900161104Allgemeiner Fehler
047000161104SERVIZIO 10:30 Uhr Service fuer folgende Bestimmungsorte nicht vorhanden
047100161104Service fuer die angegebenen Gewichte/Volumen/Kolli nicht vorhanden
047200161104Network fuer die angegebenen Gewichte/Volumen/Kolli nicht vorhanden
047300161104Service fuer die angegebenen Laender/Network nicht vorhanden
047400161104Network fuer die angegebenen Laender nicht vorhanden
047500161104Keine Verbindung zum Network/Bestimmungsorte gewählt senden
047600161104Sendungen mit C.O.D. für die angegebenen Network/Bestimmungsorte nicht vorhanden
047700161104EX WORKS Sendungen für die angegebenen Network/Bestimmungsorte nicht vorhanden
047800161104Features Expeditionen fuer dem Network/Bestimmungsorte nicht vorhanden
047900161104
