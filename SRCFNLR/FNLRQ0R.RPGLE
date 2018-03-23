000100000810     H DECEDIT('0,') DATEDIT(*DMY.)
000200101222      *--------------------------------------------------------------*
000300141202      * Finestra di Emissione Messaggio con Restituzione Forzatura   *
000400141209      * Adesso può essere anche chiamata per informare senza chiedere*
000500141209      * la Forzatura il parametro "lrq0frzIO" di I/O assume in input *
000600141209      * due valori: I=Informa e F=Forzatura                          *
000700141209      * restituisce *blk=tutto OK F=Forzato e E=Errore               *
000800101221      *--------------------------------------------------------------*
000900141202     FFNLRQ0D   CF   E             WORKSTN
001000110104     F*--------------------------------------------------------------*
001100141202     d FNlrq0ds      e ds
001200141202      *  Nella DS sono previsti il TIPO chiamata (3A) + il pgm chiamante
001300141202      *   perchè se in futuro si vogliono far fare delle cose particolari
001400141202      *   in questo programma (modificando solo questo programma) sarà
001500141202      *   possibile avere dei campi condizionanti le specifiche solo x
001600141202      *   le situazioni particolari.
001700141202      *
001800101214     D WLBDA8          DS
001900101214     D  G02DAT                 1      8  0
002000101214     D  G02INV                 9     16  0
002100101214     D  G02ERR                17     17
002200101214     D  G02TGI                18     22  0
002300101214      *
002400101214     D DATPAR          DS
002500101214     D  GIODAT                 1      8  0
002600101214     D  GIOINV                 9     16  0
002700101214     D  GIOTGI                17     21  0
002800141202      *
002900030730     d*---------------------------------------------------------------*
003000030730     d* campi di comodo                                               *
003100030730     d*---------------------------------------------------------------*
003200030730     d dtaeur          s               d   datfmt(*eur)
003300030730     d dtaiso          s               d
003400101213      *-----
003500110104     D SI              c                   'S'
003600110104     D NO              c                   'N'
003700101223      *-----------------------------------------------------*
003800101223     C     *ENTRY        PLIST
003900141202     C                   PARM                    FNlrq0ds
004000141202      *  carica la Finestra
004100141202     c                   exsr      iniz_WDW
004200141212      *  pulisce quanto ricevuto prima di restituire
004300141212     c                   move      *blank        lrq0frzIO
004400110105      *
004500141202      *  mostra la Finestra
004600141202     c     Video         tag
004700141202     c                   exfmt     W001FRZ
004800141202      *   F7 = Forza
004900141209     c                   IF        *inKG and *in07
005000141209     c                   move      '0'           lrq0errO
005100141209     c                   move      'F'           lrq0frzIO
005200141202      *
005300141202      *   F12= Lascia in errore  trsforma la "F" in "E"
005400141202     c                   elseIF    *inKL
005500141209     c                   move      '0'           lrq0errO
005600141209     c                   if              *in07
005700141209     c                   move      'E'           lrq0frzIO
005800141209     c                   end
005900141202     C                   end
006000141202     c  NKG
006100141202     canNKL              goto      Video
006200141202      *
006300141202     c                   seton                                        LR
006400110112      *-----------------------------------------------------*
006500110112     C* Inizializzazioni
006600110112      *-----------------------------------------------------*
006700141202     c     Iniz_WDW      begsr
006800141202      *
006900141209      *
007000141202     c                   clear                   W001FRZ
007100141212      *   se richiesta Forzatura  07 = ON
007200141212     c                   setoff                                       07
007300141212     c                   if        lrq0frzIO = 'F'
007400141212     c                   seton                                        07
007500141212     c                   end
007600141202      *
007700141202      *  imposta come anomalo l'uscita che poi verrà reimpostata
007800141202     c                   move      '1'           lrq0ERRO
007900141202      *
008000141202      * msg con la Spedizione come Intestazione
008100141202     c                   eval      des001 = 'Sped.: ' +
008200141202     c                                 %Editc(lrq0AASi:'X') +  ' ' +
008300141202     c                                 %Editc(lrq0LNPi:'X') +  ' ' +
008400141202     c                                 %Editc(lrq0NRSi:'X') +  ' ' +
008500141202     c                                 %Editc(lrq0NSPi:'X') +  '  '
008600141202      *-
008700141202     c                   eval      des002 = %subst(lrq0TXTi:001:50)
008800141202     c                   eval      des003 = %subst(lrq0TXTi:051:50)
008900141202     c                   eval      des004 = %subst(lrq0TXTi:101:50)
009000141202     c                   eval      des005 = %subst(lrq0TXTi:151:50)
009100110112      *
009200101223     c                   ENDSR
009300110112      *-----------------------------------------------------*
009400141202      * Da fare solo la prima volta
009500110112      *-----------------------------------------------------*
009600110112     c     *INZSR        begsr
009700110112      *
009800110112     C* Inverto la data del giorno
009900110112     C                   TIME                    WHHDAT           14 0
010000110112     C                   MOVEL     WHHDAT        HHMMss            6 0          *ORA/MINUTI/sec
010100110112     C                   MOVE      WHHDAT        G02DAT
010200110112     C                   MOVE      *BLANKS       G02ERR
010300110112     C                   CALL      'XSRDA8'
010400110112     C                   PARM                    WLBDA8
010500110112     C                   Z-ADD     G02INV        DATEU             8 0
010600110112     C                   move      *date         dtaeur
010700110112     C                   move      dtaeur        dtaiso
010800110112     c                   move      dtaiso        oggin             8 0
010900110112     c                   move      oggin         oggi              8
011000110112      *
011100110112     c                   endsr
011200110104     C*-----------------------------------------------------*
