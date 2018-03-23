000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200031028     F*          passaggio dati da PC ad AS x FAIS         *
000300040315     Fwfais00f  uf   E             DISK    PREFIX(£)
000400031106     FmzFAI02l  if a E           K DISK
000500031028     Fmzame05l  if   E           K DISK
000600031028     Fmzamh06l  if   E           K DISK
000700020709     fqsysprt   o    f  132        printer oflind(*inof)
000800031106     D fais            DS
000900100803     d  campo3                             LIKE(£CAMPO3)
001000100804     d  campo6                             LIKE(£CAMPO6)
001100100804     d  campo14                            LIKE(£CAMPO14)
001200100804     d  campo15                            LIKE(£CAMPO15)
001300100804     d  campo21                            LIKE(£CAMPO21)
001400100803     d  campo23                            LIKE(£CAMPO23)
001500100804     d  campo26                            LIKE(£CAMPO26)
001600100804     d  campo34                            LIKE(£campo34)
001700100804     d  campo38                            LIKE(£CAMPO38)
001800100803     d  campo40                            LIKE(£campo40)
001900031106     d  rrnf                         15  0
002000100803     d comtel          s                   like(amhtel)
002100100803     d com08           s                   like(amhdta)
002200031031     d com2            s                   like(amhtip)
002300031031     d faidtms         s                   like(faidtm)
002400020709     d dataiso         s               d
002500020709     d dataeur         s               d   datfmt(*eur)
002600031028     d esito           s              1  0 inz
002700031028     c* estraggo solo i passaggi telepass e il canone telepass
002800100803     c* campo3 = PE => passaggi
002900100803     c* campo3 = 22 => canone
003000100803     c* campo5 = AT => telepass
003100031028     C/EXEC SQL
003200100804     C+ DECLARE A1 CURSOR FOR SELECT CAMPO3, CAMPO6, CAMPO14, CAMPO15,
003300100804     C+ CAMPO21, CAMPO23, CAMPO26, CAMPO34, campo38, campo40,
003400100804     C+ rrn(wfais00f) FROM wfais00f WHERE ((CAMPO3 = 'PE' and campo5 =
003500100804     C+ 'AT') or campo3 = '22') and campo25 ='+'
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
007200100803     c                   movel(p)  campo6        faicod
007300040211     c* data rifornimento per i canoni non si sa se c'è oppure no
007400100804     c                   if        campo14 <> ' '
007500100804     c                   move      campo14       com8              8 0
007600100803     c                   move      com8          dataiso
007700100803     c                   move      dataiso       faidtm
007800040211     c                   end
007900031029     c                   if        faicod <> *blanks and faicod <>'0'
008000040211     c                             and faidtm <> ' '
008100031031     c* reperisco targa
008200031028     c                   exsr      srame
008300031028     c                   if        faitga = *blanks
008400031028     c                   seton                                        03
008500031106     c                   end
008600031028     c* costo passaggio
008700100804     c                   move      campo26       com13            13 5
008800100903     c                   z-add     com13         faisco
008900100903     c                   z-add     com13         faiimp
009000031028     c* passaggio esente
009100100804     c                   move      campo23       com13            13 5
009200100804     c                   move      com13         faiese
009300031028     c* tipo movimento PE/37
009400100803     c                   movel     campo3        faitmv
009500031028     c* classe passaggio
009600100803     c                   movel     campo38       faicla
009700031028     c* imposto i dati che sono presenti solo per i passaggi e no per il
009800031028     c* canone
009900100803     c                   if        campo3 = 'PE'
010000031028     c* ora rifornimento
010100100804     c                   move      campo15       faioram
010200031028     c* descrizione passaggio
010300100804     c                   movel     campo21       faides
010400031028     c                   end
010500031028     c* non deve esistere già il record
010600031028     c                   setoff                                       01
010700031028     c     kfai          setll     mzfai02l                               01
010800031106     c                   if        not *in01 and (not *in03 or (*in03 and
010900100803     c                             campo3 = '37'))
011000031028     c                   write     mzfai000
011100020715     c                   end
011200031028     c* errore se non ricevo il codice telepass
011300031028     c                   else
011400031028     c                   seton                                        02
011500031028     c                   end
011600031106     c* se esiste un errore bloccante non cancello il record
011700031106     c                   if        not *in01 and not *in02 and
011800100803     c                             (not *in03 or (*in03 and campo3 = '37'))
011900040315     c     rrnf          delete    wfais00f
012000031106     c* segnalo l'errore bloccante solo se è un pedaggio
012100031106     c* se fosse un canone lo segnalo solo in stampa xchè potrebbe essere
012200031106     c* un telepass a disposizione e non associato ad una anagrafica
012300031106     c                   else
012400031106     c                   move      '1'           error
012500031106     c                   end
012600031028     c                   if        *in01 or *in02 or *in03
012700031028     c                   exsr      stampa
012800031028     c                   end
012900031028     c                   endsr
013000031028     C**********************************************************************
013100031028     c* aggancio anagrafico automezzi
013200031028     C**********************************************************************
013300031028     C     srame         BEGSR
013400031031     c*
013500031031     c                   move      faidtm        dataiso
013600031031     c                   move      dataiso       com08
013700100803     c                   movel(p)  faicod        comtel
013800031031     C*
013900031106     c     ksto          setll     mzamh06l
014000100803     c     comtel        reade     mzamh06l
014100031106     c                   if        not %eof(mzamh06l) and
014200040212     c                             amhdta >=com08
014300031031     c                   movel     amhtga        faitga
014400031031     c                   movel     amhtgn        faitgn
014500031031     c                   else
014600100803     c     comtel        chain     mzame05l
014700031106     c                   if        %found(mzame05l)
014800031106     c                   movel     ametga        faitga
014900031106     c                   movel     ametgn        faitgn
015000031106     c                   else
015100031031     c                   clear                   faitga
015200031031     c                   clear                   faitgn
015300031031     c                   end
015400031031     c                   end
015500031028     c*
015600031028     C                   ENDSR
015700020715     C**********************************************************************
015800020715     c* stampa errori
015900020715     C**********************************************************************
016000020715     C     stampa        BEGSR
016100020709     c*
016200020709     c   of              except    testa
016300020709     c                   setoff                                       of
016400031028     c                   if        *in01 or *in02 or *in03
016500031028     c                   move      faidtm        dataiso
016600020709     c                   move      dataiso       dataeur
016700031028     c                   move      1             esito
016800031028     c   01              except    err01
016900031028     c   02              except    err02
017000031028     c   03              except    err03
017100031028     c                   setoff                                       010203
017200031028     c                   else
017300031028     c                   except    fine
017400031028     c                   end
017500020709     c*
017600020709     C                   ENDSR
017700031028     C**********************************************************************
017800031028     C     sqlerror      begsr
017900031028     C**********************************************************************
018000031028     C*
018100031028     C                   call      'X66CHGJOB'
018200031028     C                   seton                                        lr
018300031028     C*
018400031028     C                   endsr
018500020709     C**********************************************************************
018600020709     C     *inzsr        BEGSR
018700020709     C**********************************************************************
018800031106     c     *entry        plist
018900031106     c                   parm                    error             1
019000031106     c                   clear                   error
019100020709     c                   seton                                        of
019200031028     C     Kfai          KLIST                                                  *
019300031028     C                   KFLD                    faidtm                         *
019400031028     C                   KFLD                    faioram                        *
019500031028     C                   KFLD                    faicod                         *
019600031031     C     Ksto          KLIST                                                  *
019700100803     C                   KFLD                    comtel                         *
019800031106     C                   KFLD                    com08                          *
019900020709     C                   ENDSR
020000020709     OQSYSPRT   E            TESTA          2 02
020100020709     O                                         + 25 'LISTA ERRORI DURANTE'
020200020709     O                                         +  1 'IL CARICAMENTO DEL F'
020300031028     O                                         +  0 'ILE FAI SERVICE'
020400031028     O                                          113 'TRMZ80R'
020500020709     O                       UDATE              127 '  /  /  '
020600020709     O                       PAGE1         Z    132
020700031028     O          e            err01       1
020800040226     O                                         +  0 'Esiste un movimento'
020900040226     O                       faitmv            +  1
021000040226     O                                         +  1 'già caricato pre'
021100031028     O                                         +  0 'cedentemente tessera '
021200031028     o                       faicod            +  1
021300020709     O                                         +  1 'data'
021400031028     o                       dataeur           +  1
021500031028     O          e            err02       1
021600040226     O                                         +  0 'Esiste un movimento'
021700040226     O                       faitmv            +  1
021800100803     O                                         +  1 'senza cod.tessera'
021900031028     O                                         +  1 'datato'
022000031028     o                       dataeur           +  1
022100031028     O          e            err03       1
022200040226     O                                         +  0 'Esiste un movimento'
022300040226     O                       faitmv            +  1
022400100803     O                                         +  1 'con tessera '
022500031028     O                                         +  0 'inesistente nell''anagrafi'
022600040226     O                                         +  0 'co. Telep.'
022700031028     o                       faicod            +  1
022800040226     o                                         +  1 'Targa FAIS.'
022900100803     o                       campo40           +  1
023000031028     O                                         +  1 'data'
023100031028     o                       dataeur           +  1
023200021025     O          e            fine        1
023300021025     O                                         + 50 '*** NESSUNA ANOMALIA ***'
