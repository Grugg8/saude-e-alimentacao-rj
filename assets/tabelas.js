// Envolve cada tabela em um contêiner com rolagem horizontal.
// Sem isso, tabelas de cinco colunas estouram a largura no celular.
document.addEventListener("DOMContentLoaded", function () {
  var tabelas = document.querySelectorAll("#quarto-document-content table");
  tabelas.forEach(function (t) {
    if (t.parentElement && t.parentElement.classList.contains("tabela-scroll")) return;
    var caixa = document.createElement("div");
    caixa.className = "tabela-scroll";
    t.parentNode.insertBefore(caixa, t);
    caixa.appendChild(t);
  });
});
