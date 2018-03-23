000100120904     H* Gestione opzione SC su Gestione Terminal
000200120904     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000300120904     FAZCAE01L  UF A E           K DISK
000400120904     D AZCAEDS       e ds                  extname(AZCAE00F)
000500120904     D Sav_CAE       e ds                  extname(AZCAE00F)
000600120904     D                                     prefix(SAV_)
000700120904     D NewTFE          s                   like(CAETFE)
000800120904     D NewDSc          s                   like(CAEDSc)
000900120904     D NewNewDSc       s                   like(CAEDSc)
001000120904     D Rip18R1         s              1
001100120904     D Err18R1         s              1
001200120904     C*
001300120904     C                   EVAL      Err18R1 = '0'
001400120904     C*
001500120904     C* leggo l'attuale record
001600120904     C     K04CAE01      CHAIN     AZCAE000
001700120904     C                   IF        not %found
001800120904     C                   EVAL      Err18R1 = '1'
001900120904     C                   RETURN
002000120904     C                   ENDIF
002100120904     C*
002200120904     C* salvo il record letto
002300120904     C                   EVAL      Sav_CAE000 = AZCAEDS
002400120904     C*
002500120904     C* modifico l'attuale data scadenza con quella ricevuta
002600120904     C                   EVAL      CAEDSc = NewDSc
002700120904     C                   UPDATE    AZCAE000
002800120904     C*
002900120904     C* scrivo una nuova associazione con:
003000120904     C* - terminal = quello indicato a video
003100120904     C* - data decorrenza = la scadenza impostata + 1
003200120904     C* - data scadenza = quella indicata a video come data scadenza nuova associazione
003300120904     C                   EVAL      CAETFE = NewTFE
003400120904     C                   EVAL      CAEDDe = %dec(%date(NewDSc) + 1)
003500120904     C                   EVAL      CAEDSc = NewNewDSc
003600120904     C                   Z-ADD     *ZEROS        CAEDTR                         *DATA TRASMISSIONE
003700120904     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
003800120904     C                   MOVEL     *BLANKS       CAEATB                         *ANNULLAMENTO
003900120904     C                   WRITE     AZCAE000
004000120904     C*
004100120904     C* se richiesto il ripristino associazione precedente,
004200120904     C* scrivo una nuova associazione con:
004300120904     C* - dati di partenza, arrivo e terminal = quelli su sfl
004400120904     C* - data decorrenza = la scadenza impostata sulla nuova associazione + 1
004500120904     C* - data scadenza = 31/12/2039
004600120904     C                   IF        Rip18R1 = 'SI'
004700120904     C                   EVAL      AZCAEDS = Sav_CAE000
004800120904     C                   EVAL      CAEDDe = %dec(%date(NewNewDSc) + 1)
004900120904     C                   EVAL      CAEDSc = 20391231
005000120904     C                   Z-ADD     *ZEROS        CAEDTR                         *DATA TRASMISSIONE
005100120904     C                   MOVEL     *BLANKS       CAEFTR                         *FLAG TRASMISSIONE
005200120904     C                   MOVEL     *BLANKS       CAEATB                         *ANNULLAMENTO
005300120904     C                   WRITE     AZCAE000
005400120904     C                   ENDIF
005500120904     C*
005600120904     C                   SETON                                        RT
005700120904     C                   RETURN
005800120904     C
005900120904     C     *inzsr        BEGSR
006000120904     C*
006100120904     C     *ENTRY        PLIST
006200120904     C                   PARM                    CAEEPA
006300120904     C                   PARM                    CAETFP
006400120904     C                   PARM                    CAETFA
006500120904     C                   PARM                    CAEDDE
006600120904     C                   PARM                    NewTFE
006700120904     C                   PARM                    NewDSc
006800120904     C                   PARM                    NewNewDSc
006900120904     C                   PARM                    Rip18R1
007000120904     C                   PARM                    Err18R1
007100120904     C*
007200120904     C* LETTURA AZCAE01L - COMPLETA
007300120904     C     K04CAE01      KLIST
007400120904     C                   KFLD                    CAEEPA                         *PARTENZA/ARRIVO
007500120904     C                   KFLD                    CAETFP                         *TERMINAL PARTENZA
007600120904     C                   KFLD                    CAETFA                         *TERMINAL ARRIVO
007700120904     C                   KFLD                    CAEDDE                         *DECORRENZA
007800120904     C*
007900120904     C                   ENDSR
