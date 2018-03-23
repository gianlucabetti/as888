000100120904     H* Gestione opzione SC su Gestione Terminal
000200120904     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000300131015     FAZCAE01L  UF   E           K DISK
000400131015     D tisi00ds      e ds
000500131015     D kpjba         e ds
000600120904     C*
000700131015     C                   clear                   d00err
000800131015     C                   clear                   d00msg
000900131015     C                   EVAL      CAEEPA =      d00epa
001000131015     C                   EVAL      CAETFP =      d00etp
001100131015     C                   EVAL      CAETFA =      d00eta
001200131015     C                   EVAL      CAEDDE =      d00edd
001300120904     C*
001400120904     C* leggo l'attuale record
001500120904     C     K04CAE01      CHAIN     AZCAE000
001600120904     C                   IF        not %found
001700131015     C                   EVAL      d00err  = '1'
001800120904     C                   ENDIF
001900131015     c* sottaggo 1 alla data decorrenza per avere la scadelza precedente
002000131015     c                   clear                   prec_dsc          8 0
002100131015     C                   EVAL      prec_dsc = %dec(%date(caedde) - %days(1))
002200131015     c* sommo 1 alla data decorrenza
002300131015     C                   EVAL      CAEDDe = %dec(%date(caedde) + %days(1))
002400131015     c                   update    azcae000
002500131015     c*
002600131015     C     K04CAE02      readpe    AZCAE000
002700131015     c                   if        not %eof(azcae01l)  and caedsc=prec_dsc
002800131015     C                   EVAL      CAEDsc = %dec(%date(caedsc) + %days(1))
002900131015     c                   update    azcae000
003000131015     c                   else
003100131015     c
003200131015     c*Segnalo errore solo se il record precedente non ha la scadenza attaccata
003300131015     c* se non lo trovassi vuol dire che esiste solo 1 record e quindi e' ok
003400131015     c                   if        not %eof(azcae01l)  and caedsc<>prec_dsc
003500131015     C                   EVAL      d00err  = '1'
003600131015     c                   unlock    azcae01l
003700131015     c                   endif
003800131015     c                   endif
003900131015     c
004000120904     C*
004100131015     C                   SETON                                        lr
004200120904     C                   RETURN
004300120904     C
004400131015     c*-------------------------------------------------------------------------------------------
004500120904     C     *inzsr        BEGSR
004600120904     C*
004700120904     C     *ENTRY        PLIST
004800131015     C                   PARM                    kpjba
004900131015     C                   PARM                    tisi00ds
005000120904     C*
005100120904     C* LETTURA AZCAE01L - COMPLETA
005200120904     C     K04CAE01      KLIST
005300120904     C                   KFLD                    CAEEPA                         *PARTENZA/ARRIVO
005400120904     C                   KFLD                    CAETFP                         *TERMINAL PARTENZA
005500120904     C                   KFLD                    CAETFA                         *TERMINAL ARRIVO
005600120904     C                   KFLD                    CAEDDE                         *DECORRENZA
005700131015     C     K04CAE02      KLIST
005800131015     C                   KFLD                    CAEEPA                         *PARTENZA/ARRIVO
005900131015     C                   KFLD                    CAETFP                         *TERMINAL PARTENZA
006000131015     C                   KFLD                    CAETFA                         *TERMINAL ARRIVO
006100120904     C*
006200120904     C                   ENDSR
