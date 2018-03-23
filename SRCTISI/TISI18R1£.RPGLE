000100120904     H* Gestione opzione SC su Gestione Terminal
000200120904     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000300120904     FAZCAE01L  UF A E           K DISK
000400120904     D AZCAEDS       e ds                  extname(AZCAE00F)
000500120904     D Sav_CAE       e ds                  extname(AZCAE00F)
000600120904     D                                     prefix(SAV_)
000700120904     D EPA18R1         s                   like(CAEEPA)
000800120904     D TFP18R1         s                   like(CAETFP)
000900120904     D TFA18R1         s                   like(CAETFA)
001000120904     D DDE18R1         s                   like(CAEDDE)
001100120904     D NewTFE          s                   like(CAETFE)
001200120904     D NewDSc          s                   like(CAEDSc)
001300120904     D NewNewDSc       s                   like(CAEDSc)
001400120904     D Rip18R1         s              2
001500120904     D Err18R1         s              1
001600120904     C*
001700120904     C                   EVAL      Err18R1 = '0'
001800120904     C                   EVAL      CAEEPA =      EPA18R1
001900120904     C                   EVAL      CAETFP =      TFP18R1
002000120904     C                   EVAL      CAETFA =      TFA18R1
002100120904     C                   EVAL      CAEDDE =      DDE18R1
002200120904     C*
002300120904     C* leggo l'attuale record
002400120904     C     K04CAE01      CHAIN     AZCAE000
002500120904     C                   IF        not %found
002600120904     C                   EVAL      Err18R1 = '1'
002700120904     C                   RETURN
002800120904     C                   ENDIF
002900120904     C*
003000120904     C* salvo il record letto
003100120904     C                   EVAL      Sav_CAE = AZCAEDS
003200120904     C*
003300120904     C* modifico l'attuale data scadenza con quella ricevuta
003400120904     C                   EVAL      CAEDSc = NewDSc
003500120904     C                   UPDATE    AZCAE000
003600120904     C*
003700120904     C* scrivo una nuova associazione con:
003800120904     C* - terminal = quello indicato a video
003900120904     C* - data decorrenza = la scadenza impostata + 1
004000120904     C* - data scadenza = quella indicata a video come data scadenza nuova associazione
004100120904     C                   EVAL      CAETFE = NewTFE
004200120904     C                   EVAL      CAEDDe = %dec(%date(NewDSc) + %days(1))
004300120904     C                   EVAL      CAEDSc = NewNewDSc
004400120904     C                   Z-ADD     *ZEROS        CAEDTR                         *DATA TRASMISSIONE
004500120904     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
004600120904     C                   MOVEL     *BLANKS       CAEATB                         *ANNULLAMENTO
004700120904     C                   WRITE     AZCAE000
004800120904     C*
004900120904     C* se richiesto il ripristino associazione precedente,
005000120904     C* scrivo una nuova associazione con:
005100120904     C* - dati di partenza, arrivo e terminal = quelli su sfl
005200120904     C* - data decorrenza = la scadenza impostata sulla nuova associazione + 1
005300120904     C* - data scadenza = 31/12/2039
005400120904     C                   IF        Rip18R1 = 'SI'
005500120904     C                   EVAL      AZCAEDS = Sav_CAE
005600120904     C                   EVAL      CAEDDe = %dec(%date(NewNewDSc) + %days(1))
005700120904     C                   EVAL      CAEDSc = 20391231
005800120904     C                   Z-ADD     *ZEROS        CAEDTR                         *DATA TRASMISSIONE
005900120904     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
006000120904     C                   MOVEL     *BLANKS       CAEATB                         *ANNULLAMENTO
006100120904     C                   WRITE     AZCAE000
006200120904     C                   ENDIF
006300120904     C*
006400120904     C                   SETON                                        RT
006500120904     C                   RETURN
006600120904     C
006700120904     C     *inzsr        BEGSR
006800120904     C*
006900120904     C     *ENTRY        PLIST
007000120904     C                   PARM                    EPA18R1
007100120904     C                   PARM                    TFP18R1
007200120904     C                   PARM                    TFA18R1
007300120904     C                   PARM                    DDE18R1
007400120904     C                   PARM                    NewTFE
007500120904     C                   PARM                    NewDSc
007600120904     C                   PARM                    NewNewDSc
007700120904     C                   PARM                    Rip18R1
007800120904     C                   PARM                    Err18R1
007900120904     C*
008000120904     C* LETTURA AZCAE01L - COMPLETA
008100120904     C     K04CAE01      KLIST
008200120904     C                   KFLD                    CAEEPA                         *PARTENZA/ARRIVO
008300120904     C                   KFLD                    CAETFP                         *TERMINAL PARTENZA
008400120904     C                   KFLD                    CAETFA                         *TERMINAL ARRIVO
008500120904     C                   KFLD                    CAEDDE                         *DECORRENZA
008600120904     C*
008700120904     C                   ENDSR
