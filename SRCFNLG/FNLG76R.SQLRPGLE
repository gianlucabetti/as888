000100930810     H DECEDIT('0,') DATEDIT(*DMY.)
000200930810     H* FLFG76R *----------------------------------------------------*
000300930810     H*                                                              *
000400930810     H*  - STATISTICA GIACENZE IN PARTENZA - CONTROLLO UFF. GIAC. -  *
000500930810     H*                     *- LANCIO -*                             *
000600150105     H*  Da lanciare il Martedì per calcolare la settimana passata   *
000700141216     H*  Saldificandola sul FLFG765F                                 *
000800940224     H*--------------------------------------------------------------*
000900141217     D KPJBA         E DS
001000141217     D FLFG765F      E DS
001100141217      **
001200141216     F* DS *--
001300930810     D PARAM           DS
001400941013     D  DAL                    1      8  0
001500941013     D  AL                     9     16  0
001600141217     D  ANNOWW                17     22  0
001700941013     D  FIL                   23     25  0
001800141216      *
001900141217     D Da_FG765        DS
002000141217     D  FG7Anno4               1      4  0
002100141217     D  FG7Secolo              1      2  0
002200141217     D  FG7Anno                3      4  0
002300141217     D  FG7Week                5      6  0
002400141217     D  FG7AnnoWeek            1      6  0
002500141216      **
002600141217     D DATECNV0F     E DS
002700141217      **
002800141217     D da_lunedi       S               D
002900141217     D a_domenica      S               D
003000150105     D fare_da_martedi...
003100150105     d                 s               D
003200141217     D oggi            S               D
003300141217     D Anno            S              4  0
003400150107     D AnnoDa          S              4  0
003500150107     D AnnoA           S              4  0
003600141217     D Week            S              2  0
003700150105     D Errore          S              1  0
003800150105     D Tipo_di_Errore  S             40
003900141217      *
004000141217     d DataISO         s               d   datfmt(*iso)
004100141217     d DataEUR         s               d   datfmt(*eur)
004200141216      **
004300050301     D tibs56ds      E DS
004400050301     d  lin                    7    756  0 dim(250)
004500930810     D*-------------
004600930810     D* DS PER ROUTINE 'XSRDAT'
004700930810     D*-------------
004800941013     D WLBDA8          DS
004900941013     D  G02DAT                 1      8  0
005000941013     D  G02INV                 9     16  0
005100941013     D  G02ERR                17     17
005200941013     D  G02TGI                18     22  0
005300931125     D*-------------
005400931125     D* DS PER ROUTINE 'XSRGIO'
005500931125     D*-------------
005600941013     D WGIDA8          DS
005700941013     D  GIODAT                 1      8  0
005800941013     D  GIOINV                 9     16  0
005900941013     D  GIOTGI                17     21  0
006000900423     C*---------------------------------------------------------------*
006100150105     C                   SETON                                        LR
006200150105      *
006300141216      ***  prende l'ultimo ANNO/SETTIMANA elaborato x confrontarlo
006400150105      ***  e calcolare gli estremi della settimana seguente da Saldificare
006500141216
006600141216     C/EXEC SQL
006700141216     C+ SELECT    max(FG7AAS * 100+ FG7PER)
006800141217     C+ INTO      :FG7AnnoWeek
006900141216     C+ FROM FLFG765L1
007000141216     C/END-EXEC
007100141218      ***     x Errori  invio MAIL
007200150105     c                   if        sqlCOD <> 0
007300150105     c                   eval      errore = 1
007400150105     c                   exsr      invia_mail
007500150105     c                   return
007600141218     c                   end
007700141217      ***
007800141217      ***  Data una settimana dell'Anno
007900141217      ***    ricava il primo e ultimo giorno della settimana
008000141217      ***       AAAAWW --> lunedì e domenica da impostare
008100141217      ***   aggiungendo 7 prende quelli della settimana seguente
008200150105      ***   aggiungendo 15 controlla se è il martedì in cui deve
008300141217      ***   saldificare la settimana passata
008400150105      ***   quindi compara il martedì se coincide con UDATE
008500141217
008600141217     C/EXEC SQL
008700141217     C+ SELECT DISTINCT
008800141217     C+   date(days(DC_WEEK_STARTING_DATE)+7),
008900141217     C+   date(days(DC_WEEK_ENDING_DATE)+7),
009000150105     C+   date(days(DC_WEEK_STARTING_DATE)+15),
009100141217     C+   curdate()
009200150105     C+ INTO  :da_Lunedi, :a_domenica, :fare_da_martedi, :oggi
009300141217     C+ FROM DATECNV0F
009400141217     C+ WHERE DC_WOY_ISO= :FG7Week
009500141217     C+              and
009600180102     C+       DC_YOY_ISO= :FG7Anno4
009700141217     C/END-EXEC
009800141218      ***     x Errori  invio MAIL
009900150105     c                   if        sqlCOD <> 0
010000150105     c                   eval      errore = 2
010100150105     c                   exsr      invia_mail
010200150105     c                   return
010300141218     c                   end
010400141217      *
010500141217      ***  Con la data di OGGI
010600141217      ***   controlla che sia il Lunedì della settimana successiva
010700141217      ***    per lanciare l'elaborazione settimanale
010800141217      ***
010900150105     c                   IF        oggi >= fare_da_martedi
011000141217      *
011100141217      **  se è rimasto indietro con il (maggiore) si può rimettere in pari
011200141217      **  saldificando la settimana così come è.
011300141217      **
011400141217      **   Deve però calcolare la week della settimana di elaborazione
011500141217
011600141217     C/EXEC SQL
011700141217     C+ SELECT DISTINCT
011800141217     C+   DC_WOY_ISO,
011900150107     C+   YEAR(DC_WEEK_STARTING_DATE),
012000150107     C+   YEAR(DC_WEEK_ENDING_DATE)
012100150108     C+ INTO  :Week, :AnnoDa, :AnnoA
012200141217     C+ FROM DATECNV0F
012300141217     C+ WHERE DC_WEEK_STARTING_DATE = :da_Lunedi
012400141217     C/END-EXEC
012500141218      ***     x Errori  invio MAIL
012600150105     c                   if        sqlCOD <> 0
012700150105     c                   eval      errore = 3
012800150105     c                   exsr      invia_mail
012900150105     c                   return
013000141218     c                   end
013100141216      ***
013200141217      ***  La settimana critica rimane quella fra la fine e l'inizio anno
013300150107      ***  per risolvere quale Anno prendere occorre ragionare sulla 1 settimana
013400150107      ***   che deve prendere sempre l'anno dalla data della domenica al
013500150107      ***   contrario di tutte le altre volte.
013600141216
013700150107     c                   eval      anno = annoDa
013800150107     c                   if        week = 1
013900150107     c                   eval      anno = annoA
014000141217     c                   end
014100141216      ***
014200141217     c                   move      da_lunedi     dal
014300141217     c                   move      a_domenica    al
014400141218     C                   eval        AnnoWW = Anno*100 +week
014500141216      ***
014600050406     c* loop su tutti i po
014700050406     c                   do        250           i                 3 0
014800050406     c                   if        lin(i) = 0
014900050406     c                   leave
015000050406     c                   endif
015100050406     c*
015200050406     c                   if        lin(i) = 046
015300050406     c                   iter
015400050406     c                   endif
015500050406     c                   eval      fil = lin(i)
015600930810     C                   MOVEL     PARAM         KPJBU
015700930810     C* ESTRAZIONE
015800941013     C                   CALL      'FNLG77R'
015900930810     C                   PARM                    KPJBA
016000141217     C                   endDO
016100141217     C*
016200050406     C                   ENDIF
016300141217     C*
016400150105     C                   RETURN
016500930804     C*------------------------------------------------------------------------*
016600930803     C* SUBROUTINE INIZIALE
016700930803     C*-----------
016800930803     C     *INZSR        BEGSR
016900941013     C*
017000930803     C     *ENTRY        PLIST
017100930803     C                   PARM                    KPJBA
017200050301     C                   Z-ADD     1             CODUT             1 0
017300141216     C*
017400050301     C* CARICO I SIMFEL dell'as
017500050301     c                   clear                   tibs56ds
017600050301     c                   eval      i56ppo=046
017700050301     c                   eval      i56mod='AZI'
017800050301     c                   call      'TIBS56R'
017900050301     c                   parm                    tibs56ds
018000141216     C*
018100930810     C** GIRO LA DATA DEL GIORNO
018200941013     C                   TIME                    WHHDAT           14 0
018300941013     C                   MOVE      WHHDAT        G02DAT
018400941013     C                   CALL      'XSRDA8'
018500941013     C                   PARM                    WLBDA8
018600941013     C                   Z-ADD     G02INV        DATEU             8 0
018700000111     C                   Z-ADD     G02INV        OGGI6             6 0
018800930811     C*
018900930803     C                   ENDSR
019000150105     C*------------------------------------------------------------------------*
019100150105     C* INVIA Mail per ERRORE da SQL
019200150105     C*------------------------------------------------------------------------*
019300150105     C     Invia_Mail    BEGSR
019400150105     C*
019500150105     c                   if        errore = 1
019600150105     c                   eval      Tipo_di_Errore = 'lettura del -
019700150105     c                             file FLFG765F'
019800150105     c                   elseif    errore = 2
019900150105     c                   eval      Tipo_di_Errore = 'calcolo estremi: -
020000150105     c                             Lunedi e Domenica dal DATECNV0F'
020100150105     c                   elseif    errore = 3
020200150105     c                   eval      Tipo_di_Errore = 'calcolo anno week -
020300150105     c                             precedente dal DATECNV0F'
020400150105     c                   end
020500150105     C*
020600150105      * invia segnalazione via mail
020700150105     c                   eval      emlto = 'CedAlert@brt.it'
020800150105     c                   eval      emlcc = *blank
020900150105     c                   eval      emogg = 'FNLG76R Errore SQL da ver-
021000150105     c                             ificare '
021100150105     C                   eval      emmsg = 'SQLCOD = ' + %editc(SQLCOD:'K')
021200150105     c                                   + ' Errore durante: '
021300150105     c                                   + Tipo_di_errore
021400150105     c
021500150105     C                   CALL      'TIS701C1'
021600150105     C                   PARM                    emlto           253
021700150105     C                   PARM                    emlcc           253
021800150105     C                   PARM                    emogg            44
021900150105     C                   PARM                    emmsg          5000
022000150105     C                   PARM                    emesito           1
022100150105     C*
022200150105     C                   ENDSR
022300150105     C*------------------------------------------------------------------------*
