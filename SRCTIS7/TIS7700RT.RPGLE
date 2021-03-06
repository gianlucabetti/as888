000100090827     ***************************************************************************
000200090827     **
000300090827     **
000400090827     ***************************************************************************
000500170823     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000600090831
000700090831     ***************************************************************************
000800090831     **
000900090831     ** File.
001000090831     **
001100090831     ***************************************************************************
001200090831
001300090827     ***************************************************************************
001400090827     **
001500090827     ** Costanti.
001600090827     **
001700090827     ***************************************************************************
001800090827
001900090827     ***************************************************************************
002000090827     **
002100090827     ** Prototipi.
002200090827     **
002300090827     ***************************************************************************
002400090914     D/COPY GAITRASRC/SRCPROTOPR,TIS7700R
002500090827
002600090827     ***************************************************************************
002700090827     **
002800090827     ** Strutture dati.
002900090827     **
003000090827     ***************************************************************************
003100090914     D cndizion      E DS                  QUALIFIED
003200090914     D                                     BASED(nullPtr)
003300090914     D tis7700i0     E DS                  QUALIFIED
003400090914     D                                     INZ(*EXTDFT)
003500090914     D tis7700o0     E DS                  QUALIFIED
003600170823     D pInAas          S                   LIKE(cndizion.$aas)
003700170823     D pInLnp          S                   LIKE(cndizion.$lnp)
003800170823     D pInNrs          S                   LIKE(cndizion.$nrs)
003900170823     D pInNsp          S                   LIKE(cndizion.$nsp)
004000170823     D pInTbl          S                   LIKE(cndizion.$tbl)
004100170823     D pInKsu          S                   LIKE(cndizion.$ksu)
004200170823     D pInSun          S                   LIKE(cndizion.$sun)
004300170823     D flgKscCcm       S              1A
004400170823     D wCallrpyEsito   S             10I 0 INZ(*zeros)
004500090901     D*
004600090827
004700090827     ***************************************************************************
004800090827     **
004900090827     ** Campi.
005000090827     **
005100090827     ***************************************************************************
005200090827
005300090827     ***************************************************************************
005400090827     **
005500090827     ** Parametri.
005600090828     **
005700090827     **
005800090827     ***************************************************************************
005900090827
006000090827     ***************************************************************************
006100090827     **
006200090827     ** Main.
006300090827     **
006400090827     ***************************************************************************
006500090827
006600090827      /FREE
006700090827
006800090914          RESET tis7700i0;
006900170823          tis7700i0.aas = 2017;
007000170823          tis7700i0.lnp = 080;
007100170823          tis7700i0.nrs = 00;
007200170823          tis7700i0.nsp = 0050798;
007300170823          tis7700i0.tbl = *blanks;
007400170823          tis7700i0.ksu = '01020004';
007500170823          tis7700i0.sun = '000000025';
007600090914          // Se bolla nn in gestione => esco con errore
007700090914          Selettore_bolla_subUnificante('CHKBOLLA'
007800090915                    : wCallrpyEsito
007900090914                    : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
008000090914                    : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0));
008100090914
008200090915          IF wCallrpyEsito >= *zeros AND tis7700o0.bollValida = *ON;
008300090914             // Se bolla in gestione => utilizzo il flag KSC/CCM reperito
008400170823             flgKscCcm = tis7700o0.flg_kscccm;
008500090914
008600090914          ELSE;
008700090914
008800090914          ENDIF;
008900170823
009000170823          *INLR = *on;
009100090831
009200090827      /END-FREE
009300090831
