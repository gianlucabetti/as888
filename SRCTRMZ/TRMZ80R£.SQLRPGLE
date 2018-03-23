000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200031028     F*          passaggio dati da PC ad AS x FAIS         *
000300040315     Fwfais00f  uf   E             DISK    PREFIX(£)
000500031106     FmzFAI02l  if a E           K DISK
000600031028     Fmzame05l  if   E           K DISK
000700031028     Fmzamh06l  if   E           K DISK
000800020709     fqsysprt   o    f  132        printer oflind(*inof)
000900031106     D fais            DS
001000031106     d  campo4                             LIKE(£CAMPO4)
001100031106     d  campo5                             LIKE(£CAMPO5)
001200031106     d  campo6                             LIKE(£CAMPO6)
001300031106     d  campo7                             LIKE(£CAMPO7)
001400031106     d  campo8                             LIKE(£CAMPO8)
001500031106     d  campo9                             LIKE(£CAMPO9)
001600031106     d  campo12                            LIKE(£CAMPO12)
001700031106     d  campo13                            LIKE(£CAMPO13)
001800031106     d  campo14                            LIKE(£CAMPO14)
001900031106     d  campo15                            LIKE(£CAMPO15)
001901040226     d  campo19                            LIKE(£campo19)
002000031106     d  rrnf                         15  0
002100031031     d com08           s                   like(amhdta)
002200031031     d com2            s                   like(amhtip)
002300031031     d faidtms         s                   like(faidtm)
002400020709     d dataiso         s               d
002500020709     d dataeur         s               d   datfmt(*eur)
002600031028     d esito           s              1  0 inz
002700031028     c* estraggo solo i passaggi telepass e il canone telepass
002800031028     c* campo4 = PE => passaggi
002900031028     c* campo4 = 37 => canone
003000031028     c* campo10= AT => telepass
003100031028     C/EXEC SQL
003200031106     C+ DECLARE A1 CURSOR FOR SELECT CAMPO4, CAMPO5, CAMPO6, CAMPO7,
003300040226     C+ CAMPO8, CAMPO9, CAMPO12, CAMPO13, CAMPO14, CAMPO15, campo19,
003400040315     C+ rrn(wfais00f) FROM wfais00f WHERE (CAMPO4 = 'PE' and campo10 =
003500040315     C+ 'AT') or campo4 = '37'
003600031028     C/END-EXEC
003700031106
003800031106     C/EXEC SQL
003900031106     C+ OPEN A1
004000031106     C/END-EXEC
004100031028
004200031028     C                   dou       *inlr
004300031028
004400031028     C/EXEC SQL
004500031028     C+ FETCH NEXT FROM A1 INTO :fais
004600031028     C/END-EXEC
004700031028
004800031028     C                   select
004900031028     C                   when      sqlcod = 100
005000031028     C                   seton                                        lr
005100031028     c                   if        esito = 0
005200031028     c                   exsr      stampa
005300031028     c                   end
005400031028     C                   when      sqlcod = 0
005500031028     c* dettaglio
005600031028     c                   exsr      srwrite
005700031028     C                   other
005800031028     C                   exsr      sqlerror
005900031028     C                   endsl
006000031028     C*
006100031028     C                   enddo
006200031028
006300031028     C/EXEC SQL
006400031028     C+ CLOSE A1
006500031028     C/END-EXEC
006600031028     C**********************************************************************
006700031028     c* scrive record MZFAI00F
006800031028     C**********************************************************************
006900031028     c     srwrite       begsr
007000031028     c                   clear                   mzfai000
007100031028     c* codice tessera
007200031029     c                   movel     campo12       faicod
007201040211     c* data rifornimento per i canoni non si sa se c'è oppure no
007202040211     c                   if        campo5 <> ' '
007203040211     c                   move      campo5        faidtm
007204040211     c                   else
007206040211     c                   move      campo15       faidtm
007208040211     c                   end
007300031029     c                   if        faicod <> *blanks and faicod <>'0'
007301040211     c                             and faidtm <> ' '
007600031031     c* reperisco targa
007700031028     c                   exsr      srame
007800031028     c                   if        faitga = *blanks
007900031028     c                   seton                                        03
008000031106     c                   end
008100031028     c* costo passaggio
008200031028     c                   move      campo7        faiimp
008300031028     c* passaggio esente
008400031028     c                   move      campo8        faiese
008500031028     c* costo passaggio netto
008600031028     c                   move      campo9        faisco
008700031028     c* tipo movimento PE/37
008800031028     c                   movel     campo4        faitmv
008900031028     c* classe passaggio
009000031028     c                   movel     campo14       faicla
009100031028     c* imposto i dati che sono presenti solo per i passaggi e no per il
009200031028     c* canone
009300031028     c                   if        campo4 = 'PE'
009400031028     c* ora rifornimento
009500031028     c                   move      campo6        faioram
009600020605     c* data fattura
009700031028     c                   move      campo15       faidtf
009800031028     c* descrizione passaggio
009900031028     c                   movel     campo13       faides
010000031028     c                   end
010100031028     c* non deve esistere già il record
010200031028     c                   setoff                                       01
010300031028     c     kfai          setll     mzfai02l                               01
010400031106     c                   if        not *in01 and (not *in03 or (*in03 and
010500031106     c                             campo4 = '37'))
010600031028     c                   write     mzfai000
010700020715     c                   end
010800031028     c* errore se non ricevo il codice telepass
010900031028     c                   else
011000031028     c                   seton                                        02
011100031028     c                   end
011200031106     c* se esiste un errore bloccante non cancello il record
011300031106     c                   if        not *in01 and not *in02 and
011400031106     c                             (not *in03 or (*in03 and campo4 = '37'))
011500040315     c     rrnf          delete    wfais00f
011600031106     c* segnalo l'errore bloccante solo se è un pedaggio
011700031106     c* se fosse un canone lo segnalo solo in stampa xchè potrebbe essere
011800031106     c* un telepass a disposizione e non associato ad una anagrafica
011900031106     c                   else
012000031106     c                   move      '1'           error
012100031106     c                   end
012200031028     c                   if        *in01 or *in02 or *in03
012300031028     c                   exsr      stampa
012400031028     c                   end
012500031028     c                   endsr
012600031028     C**********************************************************************
012700031028     c* aggancio anagrafico automezzi
012800031028     C**********************************************************************
012900031028     C     srame         BEGSR
013000031031     c*
013100031031     c                   move      faidtm        dataiso
013200031031     c                   move      dataiso       com08
013300031031     C*
013400031106     c     ksto          setll     mzamh06l
013500031106     c     faicod        reade     mzamh06l
013600031106     c                   if        not %eof(mzamh06l) and
013700040212     c                             amhdta >=com08
013800031031     c                   movel     amhtga        faitga
013900031031     c                   movel     amhtgn        faitgn
014000031031     c                   else
014100031106     c     faicod        chain     mzame05l
014200031106     c                   if        %found(mzame05l)
014300031106     c                   movel     ametga        faitga
014400031106     c                   movel     ametgn        faitgn
014500031106     c                   else
014600031031     c                   clear                   faitga
014700031031     c                   clear                   faitgn
014800031031     c                   end
014900031031     c                   end
015000031028     c*
015100031028     C                   ENDSR
015200020715     C**********************************************************************
015300020715     c* stampa errori
015400020715     C**********************************************************************
015500020715     C     stampa        BEGSR
015600020709     c*
015700020709     c   of              except    testa
015800020709     c                   setoff                                       of
015900031028     c                   if        *in01 or *in02 or *in03
016000031028     c                   move      faidtm        dataiso
016100020709     c                   move      dataiso       dataeur
016200031028     c                   move      1             esito
016300031028     c   01              except    err01
016400031028     c   02              except    err02
016500031028     c   03              except    err03
016600031028     c                   setoff                                       010203
016700031028     c                   else
016800031028     c                   except    fine
016900031028     c                   end
017000020709     c*
017100020709     C                   ENDSR
017200031028     C**********************************************************************
017300031028     C     sqlerror      begsr
017400031028     C**********************************************************************
017500031028     C*
017600031028     C                   call      'X66CHGJOB'
017700031028     C                   seton                                        lr
017800031028     C*
017900031028     C                   endsr
018000020709     C**********************************************************************
018100020709     C     *inzsr        BEGSR
018200020709     C**********************************************************************
018300031106     c     *entry        plist
018400031106     c                   parm                    error             1
018500031106     c                   clear                   error
018600020709     c                   seton                                        of
018700031028     C     Kfai          KLIST                                                  *
018800031028     C                   KFLD                    faidtm                         *
018900031028     C                   KFLD                    faioram                        *
019000031028     C                   KFLD                    faicod                         *
019100031031     C     Ksto          KLIST                                                  *
019200031106     C                   KFLD                    faicod                         *
019300031106     C                   KFLD                    com08                          *
019400020709     C                   ENDSR
019500020709     OQSYSPRT   E            TESTA          2 02
019600020709     O                                         + 25 'LISTA ERRORI DURANTE'
019700020709     O                                         +  1 'IL CARICAMENTO DEL F'
019800031028     O                                         +  0 'ILE FAI SERVICE'
019900031028     O                                          113 'TRMZ80R'
020000020709     O                       UDATE              127 '  /  /  '
020100020709     O                       PAGE1         Z    132
020200031028     O          e            err01       1
020300040226     O                                         +  0 'Esiste un movimento'
020301040226     O                       faitmv            +  1
020400040226     O                                         +  1 'già caricato pre'
020500031028     O                                         +  0 'cedentemente tessera '
020600031028     o                       faicod            +  1
020700020709     O                                         +  1 'data'
020800031028     o                       dataeur           +  1
020900031028     O          e            err02       1
020901040226     O                                         +  0 'Esiste un movimento'
020902040226     O                       faitmv            +  1
021100040226     O                                         +  1 'senza codice tessera'
021300031028     O                                         +  1 'datato'
021400031028     o                       dataeur           +  1
021500031028     O          e            err03       1
021600040226     O                                         +  0 'Esiste un movimento'
021601040226     O                       faitmv            +  1
021800040226     O                                         +  1 'con cod.tessera '
021900031028     O                                         +  0 'inesistente nell''anagrafi'
022000040226     O                                         +  0 'co. Telep.'
022100031028     o                       faicod            +  1
022101040226     o                                         +  1 'Targa FAIS.'
022102040226     o                       campo19           +  1
022200031028     O                                         +  1 'data'
022300031028     o                       dataeur           +  1
022400021025     O          e            fine        1
022500021025     O                                         + 50 '*** NESSUNA ANOMALIA ***'
