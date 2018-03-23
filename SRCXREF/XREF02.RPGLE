000100990514      /TITLE Guida creazione Cross Reference
000200020306
000300990617     Fxrlib02l  up   e           k disk
000400990525
000500990922     D psds           sds
000600990922     D  parms            *PARMS
000700990525     D dltdbr          s              1
000800990525     D dltpgm          s              1
000900990601     D prmchgdate      ds            13
001000990601     D chgdatecymd             1      7  0
001100990601     D chgdateiso      s               d
001200990922     D wrktime         s               t
001300990922     D wrk1800         s               t   inz(t'18.00.00')
001400990922     D wrk0800         s               t   inz(t'08.00.00')
001500990617     D ore             s              5  0
001600990617     D analizzare      s              1    inz(*off)
001700990922     D qdayofweek      s              4
001800990601
001900990922     C                   time                    wrktime
002000990617     C* Determino se la libreria è da analizzare.
002100990617     C                   reset                   analizzare
002200990617     C                   select
002300990617     C* Sempre.
002400990617     C                   when      xrfanalisi = '1'
002500990617     C                   eval      analizzare = *on
002600990922     C* Se ho ricevuto il parametro
002700990922     C* ed è la notte fra venerdì e sabato
002800990922     C                   when      parms > 0
002900990922     C                             and
003000990922     C                             %addr(qdayofweek) <> *null
003100990922     C                             and
003200990922     C                                ((qdayofweek = '*FRI'
003300990922     C                                  and wrktime > wrk1800)
003400990922     C                                 or
003500990922     C                                 (qdayofweek = '*SAT'
003600990922     C                                  and wrktime < wrk0800))
003700990922     C                   eval      analizzare = *on
003800990617     C* Solo se modificata dopo l'ultima analisi.
003900990617     C                   when      xrfanalisi = '2'
004000990617     C                   clear                   chgdateiso
004100990601     C                   call      'XREF12'                             01
004200990601     C                   parm                    xrflib
004300990601     C                   parm      *zeros        prmchgdate
004400990601     C                   if        chgdatecymd > 0
004500990601     C                             and not *in01
004600990922     C     *cymd         move      chgdatecymd   chgdateiso
004700990922     C     wrktime       subdur    xrffineora    ore:*h
004800990617     C                   if        (chgdateiso > xrfinizdat)
004900990617     C                             or
005000990617     C                             (chgdateiso = xrfinizdat
005100990617     C                             and ore > 12)
005200990617     C                   eval      analizzare = *on
005300990617     C                   endif
005400990617     C                   endif
005500990617     C* Solo questa volta.
005600990617     C                   when      xrfanalisi = '3'
005700990617     C                   eval      analizzare = *on
005800990617     C                   endsl
005900020306
006000020306     C                   time                    xrfinizdat
006100020306     C                   time                    xrfinizora
006200020306
006300020306     ** Pre-cancellazione dei risultati della precedente analisi.
006400020306     C                   IF        Analizzare = *ON
006500020306     C                             OR
006600020306     C                             XrfAnalisi = '9'
006700020306     C                   CALL(E)   'XREF06'
006800020306     C                   PARM                    XrfLib
006900020306     C                   PARM      *ON           DltDBR
007000020306     C                   PARM      *ON           DltPgm
007100020306     C                   ENDIF
007200990524
007300990617     C* La libreria deve essere analizzata.
007400990617     C                   if        analizzare = *on
007500990525
007600990601     C* Analisi dei file e dei programmi.
007700990524     C                   if        xrfctfile = *on
007800990524     C                             or xrfctpgm = *on
007900990525     C* Creo.
008000990525     C                   call      'XREF03'                             01
008100990524     C                   parm                    xrflib
008200990524     C                   parm                    xrfctfile
008300990524     C                   parm                    xrfctpgm
008400990525     C* Normalizzo.
008500990525     C                   call      'XREF04'                             01
008600990525     C                   parm                    xrflib
008700990525     C*
008800990524     C                   endif
008900990524
009000990601     C* Analisi dei sorgenti.
009100990524     C                   if        xrfctsrc = *on
009200990525     C* Creo.
009300990524     C                   call      'XREF07'                             01
009400990524     C                   parm                    xrflib
009500990525     C*
009600990524     C                   endif
009700990524
009800990601     C* Aggiorno guida.
009900990601     C                   time                    xrffinedat
010000990601     C                   time                    xrffineora
010100990617     C                   if        xrfanalisi = '3'
010200990617     C                   eval      xrfanalisi = '0'
010300990617     C                   endif
010400990514     C                   update    xrlib000
010500990525
010600990525     C                   endif
010700990922
010800990922      /TITLE Operazioni iniziali.
010900990922     C     *inzsr        begsr
011000990922     C*
011100990922     C     *entry        plist
011200990922     C                   parm                    qdayofweek
011300990922     C*
011400990922     C                   endsr
