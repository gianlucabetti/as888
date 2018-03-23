000100150904     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000101150904
000200000000     F*-------------------------------------------------------------*
000300150908     F*- Controllo CODICE FISCALE persona fisica.                  -*
000400150904     F*- Nel caso sia errato restituisce XSTATO:                   -*
000402150904     F*  -1 ERRORE codice fiscale formalmente errato.
000403150907     F*   0 codice fiscale formalmente corretto.
000404150907     F*   1 AVVISO codice fiscale non congruente con descrizione soggetto.
000405150907     F*   2 AVVISO codice fiscale non congruente con cognome.
000406150907     F*   3 AVVISO codice fiscale non congruente con nome.
000408150907     F*   4 AVVISO codice fiscale non congruente con anno nascita.
000409150907     F*   5 AVVISO codice fiscale non congruente con mese nascita.
000410150907     F*   6 AVVISO codice fiscale non congruente con giorno nascita e genere.
000411150907     F*   7 AVVISO codice fiscale non congruente con comune nascita.
000500880704     F*-------------------------------------------------------------*
000600880704     D CF              S              1    DIM(16)                              COD.FISC.D
000700880704     D CF1             S              2    DIM(8)                               SK COMODO
000800880704     D CF2             S              1    DIM(8)                               SK COMODO
000900880704     D CF3             S              1    DIM(7)                               SK COMODO
001000880704     D CF4             S              1    DIM(36) CTDATA PERRCD(36)
001100880704     D CF5             S              2  0 DIM(36) CTDATA PERRCD(36)
001200880704     D CF6             S              2  0 DIM(36) CTDATA PERRCD(36)
001300880704     D CF7             S              2  0 DIM(26) CTDATA PERRCD(26)
001400880704     D CF8             S              1    DIM(26) CTDATA PERRCD(26)
001401150904     D cognome         S             30A
001402150904     D nome            S             30A
001403150904     D dataNascita     S               D
001404150904     D genere          S              1A
001405150904     D comuneNascita   S             34A
001406150904     D ragioneSociale  S             88A
001500150904
001600880704     C     *ENTRY        PLIST
001700150904     C                   PARM                    XCDFIS           16            1
001800150904     C                   PARM                    XSTATO            1 0          2
001801150904     ** Parametri facoltativi.
001802150907     C                   PARM                    ragioneSociale                 3
001803150907     ** Ulteriori parametri facoltativi.
001804150907     C                   PARM                    dataNascita                    4
001805150907     C                   PARM                    genere                         5
001806150907     ** Ulteriori parametri facoltativi.
001807150907     C                   PARM                    cognome                        6
001808150907     C                   PARM                    nome                           7
001809150907     C                   PARM                    comuneNascita                  8
001810150904     C*
001811150904     C                   SETON                                        LR
001900880704     C*
002000880704     C                   Z-ADD     0             XSTATO
002100880704     C                   MOVEA     XCDFIS        CF(1)
002200880704     C                   MOVEA     XCDFIS        CF1
002300070606     c                   if        XCDFIS='XXXXXXXXXXXXXXXX' or
002400070606     c                             XCDFIS='CCCCCCCCCCCCCCCC' or
002500070606     c                             XCDFIS='GGGGGGGGGGGGGGGG'
002600070606     C                   Z-SUB     1             XSTATO                         -1 = ERROR
002700150904     c                   RETURN
002800070606     c                   endif
002900880704     C*
003000880704     C* DIVIDO I CARATTERI PARI DA QUELLI DISPARI
003100880704     C                   MOVEL     CF1           CF2
003200880704     C                   MOVE      CF1           CF3
003300880704     C*
003400880704     C* CF2 - DISPARI. FACCIO IL LOOP PER LA SOMMATORIA
003500880704     C                   Z-ADD     0             K                 1 0
003600880704     C     RUN16         TAG
003700880704     C     1             ADD       K             K
003800880704     C     K             COMP      8                                  16
003900880704     C   16              GOTO      RU16
004000880704     C                   Z-ADD     1             W                 2 0
004100880704     C     CF2(K)        LOOKUP    CF4(W)                                 28
004200880704     C  N28              SETON                                        19        ON = ERROR
004300880704     C   19              GOTO      RU19
004400880704     C     CF5(W)        ADD       CFI1          CFI1              3 0          SOMMATORIA
004500880704     C                   GOTO      RUN16
004600880704     C     RU16          TAG
004700880704     C*
004800880704     C* CF3 -  PARI. FACCIO IL LOOP PER LA SOMMATORIA
004900880704     C                   Z-ADD     0             K
005000880704     C     RUN18         TAG
005100880704     C     1             ADD       K             K
005200880704     C     K             COMP      7                                  18
005300880704     C   18              GOTO      RU18
005400880704     C                   Z-ADD     1             W
005500880704     C     CF3(K)        LOOKUP    CF4(W)                                 28
005600880704     C  N28              SETON                                        19        19 = ERROR
005700880704     C   19              GOTO      RU19
005800880704     C     CF6(W)        ADD       CFI1          CFI1                           SOMMATORIA
005900880704     C                   GOTO      RUN18
006000880704     C     RU18          TAG
006100880704     C*
006200880704     C* DIVIDO LA SOMMATORIA PER 26 E SALVO IL RESTO PER IL TEST FINALE
006300880704     C     CFI1          DIV       26            CFI2              2 0
006400880704     C                   MVR                     CFI3              2 0
006500880704     C*
006600880704     C* CERCO IL CARATTERE CORRISPONDENTE AL RESTO E TESTO SE IL C.F.
006700880704     C* E' OK
006800880704     C                   Z-ADD     1             W
006900880704     C     CFI3          LOOKUP    CF7(W)                                 17
007000880704     C     CF(16)        COMP      CF8(W)                             1919      19 = ERROR
007100880704     C     RU19          TAG
007200880704     C   19              Z-SUB     1             XSTATO                         -1 = ERROR
007201150904     C   19              RETURN
007202150904
007203150904     ***************************************************************************
007204150904     **
007205150908     ** Se arrivo qui significa che il codice è formalmente corretto, quindi
007206150908     ** posso procedere con i controlli di congruenza con i dati anagrafici.
007207150908     ** Poiché i controlli non sono conformi al 100% con le regole ufficiali
007208150908     ** preferisco restituire un esito positivo, cioè avviso anziché errore.
007209150904     **
007210150904     ***************************************************************************
007211150907     C                   SELECT
007212150908     C                   WHEN      %PARMS() < 3
007213150908     C                   RETURN
007214150908     C                   WHEN      %PARMS() = 3
007215150907     C                   CALLP     ChkCongruenzaAnagrafica(xCdFiS : xStato
007216150907     C                             : ragioneSociale)
007217150907     C                   WHEN      %PARMS() = 5
007218150907     C                   CALLP     ChkCongruenzaAnagrafica(xCdFiS : xStato
007219150907     C                             : ragioneSociale : dataNascita : genere)
007220150907     C                   WHEN      %PARMS() = 8
007221150907     C                   CALLP     ChkCongruenzaAnagrafica(xCdFiS : xStato
007222150907     C                             : ragioneSociale : dataNascita : genere
007223150907     C                             : cognome : nome : comuneNascita)
007224150907     C                   ENDSL
007401150904
007600150904
007601150904     P*--------------------------------------------------
007602150904     P* Procedure name: ChkCongruenzaAnagrafica
007603150904     P* Purpose:        Controlla la congruenza tra codice e dati anagrafici.
007605150904     P* Returns:        Esito.
007606150904     P*--------------------------------------------------
007607150904     P ChkCongruenzaAnagrafica...
007608150904     P                 B
007609150904     D ChkCongruenzaAnagrafica...
007610150904     D                 PI            10I 0
007611150907     D  codiceFiscale                16A   VALUE
007612150907     D  esito                         1P 0
007613150907     D  ragioneSociale...
007614150907     D                               88A   OPTIONS(*NOPASS)
007615150907     D  dataNascita                    D   OPTIONS(*NOPASS)
007616150907     D  genere                        1A   OPTIONS(*NOPASS)
007617150907     D  cognome                      30A   OPTIONS(*NOPASS)
007618150907     D  nome                         30A   OPTIONS(*NOPASS)
007619150907     D  comuneNascita                34A   OPTIONS(*NOPASS)
007620150907
007621150907     D/COPY GAITRASRC/SRCCONST,XCDFIS
007622150904     D retField        S             10I 0
007623150907     D meseNascita     DS                  QUALIFIED
007636150904     D  mesi                          1A   DIM(12)
007637150904     D  gennaio                1      1A   INZ('A')
007638150904     D  febbraio               2      2A   INZ('B')
007639150904     D  marzo                  3      3A   INZ('C')
007640150904     D  aprile                 4      4A   INZ('D')
007641150904     D  maggio                 5      5A   INZ('E')
007642150904     D  giugno                 6      6A   INZ('H')
007643150904     D  luglio                 7      7A   INZ('L')
007644150904     D  agosto                 8      8A   INZ('M')
007645150904     D  settembre              9      9A   INZ('P')
007646150904     D  ottobre               10     10A   INZ('R')
007647150904     D  novembre              11     11A   INZ('S')
007648150904     D  dicembre              12     12A   INZ('T')
007649150904     D ggNascita       S              2S 0
007650150904     D annoNascita     DS                  QUALIFIED
007651150904     D  a1234                  1      4S 0
007652150904     D  a34                    3      4A
007653150904     D p               S              3U 0
007654150904     D codiceCatasto   S              4A
007655150904     D comuneByCodiceCatasto...
007656150904     D                 S                   LIKE(comuneNascita)
007659150904
007660150904      /FREE
007682150904
007683150908       // Controllo congruenza con ragione sociale.
007684150907       // La regola ufficiale è difficile da applicare, quindi mi limito a
007685150907       // verificare l'esistenza delle 6 lettere nel cognome e nome
007686150907       // senza considerare la posizione e la lunghezza di cognome e nome.
007687150907
007688150907       IF %PARMS() > 2 AND ragioneSociale <> *BLANK;
007694150907
007695150907         IF %SCAN(%SUBST(codiceFiscale:1:1) : ragioneSociale) = *ZERO
007696150907         OR %SCAN(%SUBST(codiceFiscale:2:1) : ragioneSociale) = *ZERO
007697150907         OR %SCAN(%SUBST(codiceFiscale:3:1) : ragioneSociale) = *ZERO
007699150907         OR %SCAN(%SUBST(codiceFiscale:4:1) : ragioneSociale) = *ZERO
007700150907         OR %SCAN(%SUBST(codiceFiscale:5:1) : ragioneSociale) = *ZERO
007701150907         OR %SCAN(%SUBST(codiceFiscale:6:1) : ragioneSociale) = *ZERO;
007703150907           esito = XCDFIS_ERRORE_RAGIONE_SOCIALE;
007704150907           RETURN XCDFIS_ERRORE_RAGIONE_SOCIALE;
007705150907         ENDIF;
007706150907
007707150907       ENDIF;
007708150907
007709150907       // Normalizzo il codice, cioè rimetto il valore numerico sostituito con
007710150907       // una lettera in caso di omocodia. La regola è: 0 = L, 1 = M, 2 = N, 3 = P,
007711150907       // 4 = Q, 5 = R, 6 = S, 7 = T, 8 = U, 9 = V.
007712150907
007713150907       FOR p = 7 TO 15;
007714150907         IF p = 7 OR p = 8 OR p = 10 OR p = 11 OR p = 13 OR p = 14 OR p = 15;
007715150907           SELECT;
007716150907             WHEN %SUBST(codiceFiscale:p:1) = 'L';
007717150907               %SUBST(codiceFiscale:p:1) = '0';
007718150907             WHEN %SUBST(codiceFiscale:p:1) = 'M';
007719150907               %SUBST(codiceFiscale:p:1) = '1';
007720150907             WHEN %SUBST(codiceFiscale:p:1) = 'N';
007721150907               %SUBST(codiceFiscale:p:1) = '2';
007722150907             WHEN %SUBST(codiceFiscale:p:1) = 'P';
007723150907               %SUBST(codiceFiscale:p:1) = '3';
007724150907             WHEN %SUBST(codiceFiscale:p:1) = 'Q';
007725150907               %SUBST(codiceFiscale:p:1) = '4';
007726150907             WHEN %SUBST(codiceFiscale:p:1) = 'R';
007727150907               %SUBST(codiceFiscale:p:1) = '5';
007728150907             WHEN %SUBST(codiceFiscale:p:1) = 'S';
007729150907               %SUBST(codiceFiscale:p:1) = '6';
007730150907             WHEN %SUBST(codiceFiscale:p:1) = 'T';
007731150907               %SUBST(codiceFiscale:p:1) = '7';
007732150907             WHEN %SUBST(codiceFiscale:p:1) = 'U';
007733150907               %SUBST(codiceFiscale:p:1) = '8';
007734150907             WHEN %SUBST(codiceFiscale:p:1) = 'V';
007735150907               %SUBST(codiceFiscale:p:1) = '9';
007736150907           ENDSL;
007737150907         ENDIF;
007738150907       ENDFOR;
007753150904
007754150904       // Controllo congruenza data di nascita.
007755150904
007756150907       IF %PARMS() > 3 AND dataNascita > *LOVAL;
007758150904
007759150904         // Anno di nascita.
007760150904
007761150904         annoNascita.a1234 = %SUBDT(dataNascita:*Y);
007762150904
007763150904         IF annoNascita.a34 <> %SUBST(codiceFiscale:7:2);
007764150907           esito = XCDFIS_ERRORE_ANNO_NASCITA;
007765150907           RETURN XCDFIS_ERRORE_ANNO_NASCITA;
007766150904         ENDIF;
007767150904
007768150904         // Mese di nascita.
007769150904
007770150904         IF meseNascita.mesi(%SUBDT(dataNascita:*M))
007771150904         <> %SUBST(codiceFiscale:9:1);
007772150907           esito = XCDFIS_ERRORE_MESE_NASCITA;
007773150907           RETURN XCDFIS_ERRORE_MESE_NASCITA;
007775150904         ENDIF;
007776150904
007777150904         // Giorno di nascita.
007778150904
007779150904         ggNascita = %SUBDT(dataNascita:*D);
007780150904
007781150904         SELECT;
007782150907           WHEN genere = XCDFIS_MASCHIO;
007783150904             IF %EDITC(ggNascita:'X') <> %SUBST(codiceFiscale:10:2);
007784150907               esito = XCDFIS_ERRORE_GIORNO_NASCITA;
007785150907               RETURN XCDFIS_ERRORE_GIORNO_NASCITA;
007787150904             ENDIF;
007788150907           WHEN genere = XCDFIS_FEMMINA;
007789150904             ggNascita += 40;
007790150904             IF %EDITC(ggNascita:'X') <> %SUBST(codiceFiscale:10:2);
007791150907               esito = XCDFIS_ERRORE_GIORNO_NASCITA;
007792150907               RETURN XCDFIS_ERRORE_GIORNO_NASCITA;
007794150904             ENDIF;
007795150907           WHEN genere <> *BLANK; // Valore non valido.
007797150907             esito = XCDFIS_ERRORE_GIORNO_NASCITA;
007798150907             RETURN XCDFIS_ERRORE_GIORNO_NASCITA;
007799150904         ENDSL;
007800150904
007801150904       ENDIF;
007802150907
007803150907       // Controllo congruenza cognome.
007804150907       // La regola ufficiale è difficile da applicare, quindi mi limito a
007805150907       // verificare l'esistenza delle 3 lettere nel cognome senza considerare
007806150907       // la posizione.
007807150907
007808150908       IF %PARMS > 5 AND cognome <> *BLANK;
007809150907
007810150907         IF %SCAN(%SUBST(codiceFiscale:1:1) : cognome) = *ZERO
007811150907         OR %SCAN(%SUBST(codiceFiscale:2:1) : cognome) = *ZERO;
007818150907           esito = XCDFIS_ERRORE_COGNOME;
007819150907           RETURN XCDFIS_ERRORE_COGNOME;
007820150907         ENDIF;
007821150907
007822150907         IF %LEN(%TRIM(cognome)) = 2;
007823150907           IF %SUBST(codiceFiscale:3:1) <> 'X';
007824150907             esito = XCDFIS_ERRORE_COGNOME;
007825150907             RETURN XCDFIS_ERRORE_COGNOME;
007826150907           ENDIF;
007828150907         ELSE;
007829150907           IF %SCAN(%SUBST(codiceFiscale:3:1) : cognome) = *ZERO;
007830150907             esito = XCDFIS_ERRORE_COGNOME;
007831150907             RETURN XCDFIS_ERRORE_COGNOME;
007832150907           ENDIF;
007833150907         ENDIF;
007834150907
007835150907         IF ragioneSociale <> *BLANK
007836150907         AND %SCAN(%TRIM(cognome) : ragioneSociale) = *ZERO;
007837150907           esito = XCDFIS_ERRORE_COGNOME;
007838150907           RETURN XCDFIS_ERRORE_COGNOME;
007839150907         ENDIF;
007840150907
007841150907       ENDIF;
007842150907
007843150907       // Controllo congruenza nome.
007844150907       // La regola ufficiale è difficile da applicare, quindi mi limito a
007845150907       // verificare l'esistenza delle 3 lettere nel nome senza considerare
007846150907       // la posizione.
007847150907
007848150908       IF %PARMS() > 6 AND nome <> *BLANK;
007849150907
007850150907         IF %SCAN(%SUBST(codiceFiscale:4:1) : nome) = *ZERO
007851150907         OR %SCAN(%SUBST(codiceFiscale:5:1) : nome) = *ZERO;
007852150907           esito = XCDFIS_ERRORE_NOME;
007853150907           RETURN XCDFIS_ERRORE_NOME;
007854150907         ENDIF;
007855150907
007856150907         IF %LEN(%TRIM(nome)) = 2;
007857150907           IF %SUBST(codiceFiscale:6:1) <> 'X';
007858150907             esito = XCDFIS_ERRORE_NOME;
007859150907             RETURN XCDFIS_ERRORE_NOME;
007860150907           ENDIF;
007861150907         ELSE;
007862150907           IF %SCAN(%SUBST(codiceFiscale:6:1) : nome) = *ZERO;
007863150907             esito = XCDFIS_ERRORE_NOME;
007864150907             RETURN XCDFIS_ERRORE_NOME;
007865150907           ENDIF;
007866150907         ENDIF;
007874150907
007875150907         IF ragioneSociale <> *BLANK
007876150907         AND %SCAN(%TRIM(nome) : ragioneSociale) = *ZERO;
007877150907           esito = XCDFIS_ERRORE_NOME;
007878150907           RETURN XCDFIS_ERRORE_NOME;
007879150907         ENDIF;
007880150907
007881150907       ENDIF;
007882150904
007883150904       // Controllo congruenza comune di nascita.
007884150904
007885150908       IF %PARMS() > 7 AND comuneNascita <> *BLANK;
007886150904
007887150904         codiceCatasto = %SUBST(codiceFiscale:12:4);
007888150904
007889150904         EXEC SQL
007890150904           SELECT LEFT(tbeUni, 34)
007891150904             INTO :comuneByCodiceCatasto
007892150904             FROM tntbe00f
007893150904            WHERE tbeCod = 'CTT' AND tbeKe1 = :codiceCatasto
007894150904              AND tbeKe2 = '' AND tbeLin = '' AND tbeSif = ''
007895150904         ;
007896150904
007897150904         IF sqlCode = 100 OR comuneNascita <> comuneByCodiceCatasto;
007898150907           esito = XCDFIS_ERRORE_COMUNE_NASCITA;
007899150907           RETURN XCDFIS_ERRORE_COMUNE_NASCITA;
007900150904         ENDIF;
007901150904
007902150904       ENDIF;
007903150904
007904150904       RETURN retField;
007905150904
007906150904      /END-FREE
007907150904     P ChkCongruenzaAnagrafica...
007908150904     P                 E
007909150904
007910150904**        CF4
0079118807040123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ
007912880704**        CF5
007913880704010005070913151719210100050709131517192102041820110306081214161022252423
008000880704**        CF6
008100880704000102030405060708090001020304050607080910111213141516171819202122232425
008200880704**        CF7
0083008807040001020304050607080910111213141516171819202122232425
008400880704**        CF8
008500880704ABCDEFGHIJKLMNOPQRSTUVWXYZ
