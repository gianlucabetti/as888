000100000000     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(*CALLER)
000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*----------------------------------------------------*
000400000000     H*
000500150306     H*
000600150306     H*       Ricezione attestazione certificazione
000700150306     H*
000800000000     F*----------------------------------------------------*
000900150306     FSDGREL00F IF   E             DISK
001000150306     FNd77018L  UF   E           K DISK
001100000000     D*----------------------------------------------------
001200000000     D* Parametri ricevuti
001300150306     D SDGSK66DS     E DS
001400000000     D*-------------
001500000000     D* Dati di ambiente ottenuti da SDGSOCDR
001600000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
001700000000     D*-------------
001800000000     D* DS Interna per dati di output di SDGSOCDR
001900000000     D XSOCDS          S           1000
002000000000     D*----------------
002100000000     D* Reperimento nome PGM
002200000000     D STATUS         SDS           333
002300000000     D  DSPGM            *PROC
002400000000     D*------------
002500000000     D* Passaggio Parametri
002600000000     D KPJBA         E DS
002700150306     D*----------------
002800150306     D* Richiamo a SDGTABDR
002900150306     D SDGTABDS      E DS                  inz
003000150306     D*----------------
003100150306     D* Scomposizione tracciato attestazione
003200150306     D SDGRELDS      E DS
003300000000     C*----------------------------------------------------*
003400000000     C*                MAIN LINE PROGRAM
003500000000     C*----------------------------------------------------*
003600000000     C*
003700000000     C                   EXSR      INZVAR
003800000000     C*
003900150306     C* Tipo Rec. R: certificazione accolta
004000150306     C* Tipo Rec. Q: certificazione respinta
004100000000     C* -----------------------------------------------------
004200000000     C* Prima lettura
004300150306     C                   read      SdgRel00f                              90
004400150205     C*
004500150306     C                   dow       *in90 = *off
004600000000     C*
004700150306     C                   If        %subst(RelRec:18:1) = 'R'
004800150306     C                   movel     RelRec        SdgRelDs
004900000000     C*
005000150306     C* Si elaborano solo le certificazioni accolte
005100150306     C                   If        RelTr   = 'R'
005200150306     C                   Exsr      ElaRec
005300150306     C                   EndIf
005400000000     C*
005500150306     C                   EndIf
005600150306     C*
005700150306     C                   read      SdgRel00f                              90
005800000000     C                   EndDo
005900000000     C*
006000000000     C* fine programma
006100000000     C                   Exsr      EndPgm
006200000000     C*
006300000000     C************************************************************
006400150306     C* Elabora certificazione
006500000000     C************************************************************
006600150306     C     ElaRec        BEGSR
006700150205     C*
006800150306     C                   eval      N77Societa = SocK66
006900150306     C                   eval      N77Acf     = AnnoK66
007000150309     C                   eval      N77Cdf     = RelCfp
007100150306     C                   eval      N77ProtCei = RelPrgCer
007200150309     C     K0477018      chain     Nd77018l
007300150306     C                   if        %found(nd77018l)
007400150306     C                   eval      N77Protoc  = RelProt
007500150310     C                   eval      N77ProtCea = RelPrgCer1
007600150306     C                   update    Nd770000
007700150306     C                   endif
007800150116     C*
007900000000     C                   ENDSR
008000000000     C************************************************************
008100000000     C* Inizializzazioni variabili
008200000000     C************************************************************
008300000000     C     InzVar        BegSr
008400000000     C*
008500150306     C                   MoveL     KPJBU         SDGSK66DS
008600000000     C*
008700150306     C                   Eval      ERRK66      = *Off
008800000000     C*
008900000000     C                   MoveL     'SOC001'      TIPXSC
009000150306     C                   MoveL     SOCK66        SOCXSC
009100000000     C*
009200000000     C                   Exsr      RepSoc
009300000000     C                   If        RTNXSC     <> '1'
009400000000     C                   MoveL     XSOCDS        SOC001
009500150306     C                   Else
009600000000     C                   Exsr      EndPgm
009700000000     C                   EndIf
009800000000     C*
009900150107     C                   EndSr
010000150306     C/EJECT
010100150224     C************************************************************
010200150224     C* OPERAZIONI INIZIALI
010300000000     C************************************************************
010400000000     C     *INZSR        BegSr
010500000000     C*
010600000000     C* Reperimento parametri
010700000000     C*
010800150107     C     *ENTRY        PLIST
010900150107     C                   PARM                    KPJBA
011000000000     C*
011100000000     C                   EndSr
011200000000     C************************************************************
011300000000     C* DEFINIZIONE KLIST
011400000000     C************************************************************
011500000000     C     DefKlist      BegSr
011600000000     C*
011700000000     C* KLIST
011800150306     C*
011900150306     C* ND77018L
012000150309     C     K0477018      KLIST
012100150306     C                   KFLD                    N77Societa
012200150306     C                   KFLD                    N77Acf
012300150309     C                   KFLD                    N77Cdf
012400150306     C                   KFLD                    N77ProtCei
012500000000     C*
012600000000     C                   EndSr
012700000000     C************************************************************
012800000000     C* Reperimento dati società
012900000000     C************************************************************
013000000000     C     RepSoc        BegSr
013100000000     C*
013200000000     C                   CALL      'SDGSOCDR'
013300000000     C                   PARM                    TIPXSC            6
013400000000     C                   PARM                    SOCXSC            3
013500000000     C                   PARM                    CDSXSC            9 0
013600000000     C                   PARM                    MODXSC            3
013700000000     C                   PARM      *Blanks       RTNXSC            1
013800000000     C                   PARM                    XSOCDS
013900000000     C                   PARM                    KPJBA
014000000000     C*
014100000000     C                   EndSr
014200000000     C************************************************************
014300000000     C* FINE PROGRAMMA
014400000000     C************************************************************
014500000000     C     EndPgm        BegSr
014600000000     C*
014700150306     C                   MoveL     SDGSK66DS     KPJBU
014800000000     C                   SETON                                        LR
014900000000     C*
015000000000     C                   RETURN
015100000000     C*
015200000000     C                   EndSr
015300000000     C************************************************************
