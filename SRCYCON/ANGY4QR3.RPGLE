000100990607     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990607     H DECEDIT('0,') DATEDIT(*DMY.)
000300990607     F*----------------------------------------------------*
000400990607     FANATB01L  UF   E           K DISK
000500990607     D*----------------------------------------------------*
000600990607      * Errore
000700990607     D ERR             S             70    DIM(1) CTDATA PERRCD(1)
000800990607      * Kpjba
000900990607     D KPJBA         E DS
001000990607     D*-------------
001100990607      * DS Società
001200990607     D SOC001        E DS                  EXTNAME(XSOC001DS)
001300990607     D*-------------
001400990607      * DS Interna per dati utente
001500990607     D XSOCDS          DS          1000
001600990607     D*-------------
001700990607      * DS per scomposizione dati tabella Y4Q
001800990607     D ANGY4QDS      E DS
001900990607      * DS del pgm
002000990607     D ANGY4QDS3     E DS
002100990607     D*----------------------------------------------------*
002200990607     C* Dati in Entrata
002300990607     C     *entry        PLIST
002400990607     C                   Parm                    Kpjba
002500990607     C                   movel     Kpjbu         Angy4qds3
002600990607     C*
002700990607     C* Reperisco i dati della società
002800990607     C                   MOVEL     'SOC001'      TIPXSC
002900990607     C                   move      D4qsoc        socxsc
003000990607     C                   CALLB     'XSOC'
003100990607     C                   PARM                    TIPXSC            6
003200990607     C                   PARM                    SOCXSC            3
003300990607     C                   PARM                    CDSXSC            9 0
003400990607     C                   PARM                    MODXSC            3
003500990607     C                   PARM      *BLANKS       RTNXSC            1
003600990607     C                   PARM                    XSOCDS
003700990607     C                   PARM                    KPJBA
003800990609     C     RTNXSC        IFNE      '1'
003900990609     C                   MOVEL     XSOCDS        SOC001
004000990609     C                   ENDIF
004100990607     C* Definisco chiave di accesso alla tabella
004200990607     C     Katb          klist
004300990607     C                   kfld                    kcod
004400990607     C                   kfld                    kazi
004500990607     C* Definisco varibili accesso
004600990607     C     *like         define    atbcod        kcod
004700990607     C     *like         define    atbazi        kazi
004800990607     C*
004900990607     C* Accedo alla tabella x reperire la data
005000990607     C                   clear                   angy4qds
005100990607     C                   movel     'Y4Q'         KCOD
005200990609     C                   movel     Xscsoc        Kazi
005300990607     C     Katb          chain     anatb01l                           31
005400990607     C                   if        not *in31
005500990607     C                   movel     atbuni        angy4qds
005600990607     C                   movel     §4qdat        d4qdat
005700990607     C* Richiamo pgm per conversione della data
005800990607     C                   movel     §4qdat        d4qdat
005900990607     C                   move      §4qdat        d4qdt8
006000990607     C                   else
006100990607     C                   movel     Err(1)        d4qmsg
006200990607     C                   end
006300990607     C*
006400990609     C                   movel     xsclin        D4Qlin
006500990609     C                   movel     xsccut        D4Qcut
006600990607     C                   movel     angy4qds3     kpjbu
006700990607     C                   seton                                        lr
006800990607**
006900990607Non esiste la tabella dei dati infragruppo nell'azienda selezionata
