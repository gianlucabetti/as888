000100020418     D ddatiute      e ds
000200020417     D azuteds       e ds                  extname(AZUTE00F)
000300020418     D tibs34ds      E DS                  inz
000400020506     D DLAT          e ds
000500020507     D MSG             S             78    DIM(01) CTDATA PERRCD(1)             MSG VIDEO
000600020506
000700020506      *-- Definire DUTE01 solo se nel programma si deve utilizzare
000800020506      *--  un livello di autorita specifico
000900020506     D DUTE01        e ds
001000990628
001100020507      *--------------------  C A L C O L O  ---------------------------
001200020506
001300020506      *-- Eseguire le seguenti specifiche solo una volta all'inizio del
001400020506      *--  programma
001500020507
001600020417     c     *dtaara       define    §azute        azuteds
001700020418     c     *dtaara       define    §datiute      ddatiute
001800020417
001900020506      *-- Se il pgm utilizza altre aree dati specificare l'istruzione
002000020509      *--  "IN" per entrambe le aree dati, AZUTEDS e DDATIUTE
002100020418     C                   in(E)     *dtaara
002200020423     C                   IF        %Error  or  RSUT = *blanks
002300020418     C                   call      'TIBS34R'
002400020418     C                   parm                    Tibs34Ds
002500020506      *-- Se il pgm utilizza altre aree dati specificare l'istruzione
002600020509      *--  "IN" per entrambe le aree dati, AZUTEDS e DDATIUTE
002700020418     C                   in        *dtaara
002800020418     c                   ENDIF
002900020424
003000020507      *-- Verifica errori e autorità profilo
003100020424     c                   SELECT
003200020506      *-- controllo se ho errori nei dati utente
003300020507      *--   sostituire *IN?? e ???msg con i campi del pgm
003400020424     c                   WHEN      DUTERR = 'E'
003500020507     c                   eval      *IN?? = *on
003600020507     c                   eval      ???msg = DUTMSG
003700020506
003800020506      *-- CONTROLLO AUTORITA'
003900020507      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
004000020506      *-- se il chiamante non richiede autorità specifica verificare
004100020506      *--   quella del profilo
004200020506      *-- se il chiamante richiede autorità specifica verificarla,
004300020506      *--  se è blank verificare quella del profilo
004400020507     c                   WHEN      UTEAUT = *blank
004500020424     c                   OTHER
004600020506
004700020506      *-----------------------------------------------------------------
004800020506      *-- Con AUTORITÀ SPECIFICA
004900020424     c                   movel     UTEFAF        DUTE01
005000020506      *--  Sostituire §UTE??? con il campo apposito
005100020424     c                   IF        §UTE??? <> *blanks
005200020424     c                   eval      wabi = §UTE???
005300020424     c                   ELSE
005400020424     c                   eval      wabi = UTEAUT
005500020424     c                   ENDIF
005600020506
005700020506      *-- Senza AUTORITÀ SPECIFICA
005800020424     c                   eval      wabi = UTEAUT
005900020424      *-----------------------------------------------------------------
006000020506
006100020506      *-- Se l'autorità assegnata è del tipo "senza abilitazione"
006200020506      *--  valutare caso per caso il da farsi, normalmente si dovrebbe
006300020506      *--  emettere un messaggio e impedire l'elaborazione
006400020424     c                   clear                   tibs02ds
006500020424     c                   movel     'C'           t02mod
006600020424     c                   movel     knsif         t02sif
006700020424     c                   movel     'LAT'         t02cod
006800020424     c                   movel(p)  wabi          t02ke1
006900020424     c                   call      'TIBS02R'
007000020424     c                   parm                    kpjba
007100020424     c                   parm                    tibs02ds
007200020424     c                   movel     t02uni        dlat
007300020424     c                   if        §latabi = 'S'
007400020507      *-- sostituire *IN??, ???msg e ?msg con i campi del pgm
007500020506     c                   eval      *IN?? = *on
007600020507     c                   eval      ???msg = ?msg(01)
007700020424     c                   endif
007800020424      *
007900020424     c                   ENDSL
008000020506
008100020506**
008200020506Utente non abilitato alla funzione
