//必要な場合ページを印刷モードにする
let i = new URL(window.location.href);
let pms = i.searchParams;

if(pms.has('embed'))
{
    document.querySelectorAll('.print-hide').forEach(element =>element.parentNode.removeChild(element));
    document.querySelectorAll('.print-visible').forEach(element =>element.classList.remove('d-none'));
    document.querySelector('.print-main').classList.remove('col-md-8');
}

//該当するクラスを置換するメソッド
function replace_class(base,to){
    document.querySelectorAll('.'+base).forEach(element => element.classList.replace(base,to));
}
function add_class(base,to){
    document.querySelectorAll('.'+base).forEach(element => element.classList.add(to));
}

//ページのコピーボタンが押されたときの処理
$('#page-url-copy-btn').on('click',function(){
    $(document.body).append("<textarea id=\"copyTarget\" style=\"position:absolute; left:-9999px; top:0px;\" readonly=\"readonly\">" +location.href+ "</textarea>");
  let obj = document.getElementById("copyTarget");
  let range = document.createRange();
  range.selectNode(obj);
  window.getSelection().addRange(range);
  document.execCommand('copy');
    this.querySelector('.'+ICON_COPY).classList.add('text-success');
    this.querySelector('.'+ICON_COPY).classList.replace(ICON_COPY,ICON_COPY_CHECK);
});
//
$('.btn-outline-secondary .switch').on('click', function() {
    this.replace_class('btn-outline-secondary','btn-secondary');
  });

$('.btn-secondary .switch').on('click', function() {
    this.replace_class('btn-secondary','btn-outline-secondary');
  });


let toggle_state_nav =   false;
let toggle_state_toc =   false;

function toggle_nav(nav){
    toggle_state_nav = !toggle_state_nav;
    if(toggle_state_nav){
        nav.classList.replace('btn-outline-secondary','btn-secondary');
    }else{
        nav.classList.replace('btn-secondary','btn-outline-secondary');
    }
}
function toggle_toc(toc){
    toggle_state_toc = !toggle_state_toc;
    if(toggle_state_toc){
        toc.classList.replace('btn-outline-secondary','btn-secondary');
    }else{
        toc.classList.replace('btn-secondary','btn-outline-secondary');
    }
}

function search(ismobile){
    let query=document.getElementById('searchbox').value;
    if(ismobile){
        query=document.getElementById('searchbox_mobile').value;
    }
    let target = document.getElementById('mkdocs-search-query');
    target.value=query;
    let e = new Event('keyup');
    target.dispatchEvent(e);
    let searchModal = new bootstrap.Modal(document.getElementById('searchModal'), {
        keyboard: false
      });
    searchModal.show();
}

let url = new URL(window.location.href);
let params = url.searchParams;

var get_query = params.get('q');

document.getElementById('searchbox').value=get_query;
document.getElementById('searchbox_mobile').value=get_query;

$('.nav_cop').click(function() {
    $(this).next().collapse('toggle');
});


function print_view(){
    var url = new URL(window.location.href);
	url.searchParams.append('embed','');
	window.open(url);
}

function print_normal(){
    var url = new URL(window.location.href);
	url.searchParams.delete('embed');
	window.open(url);
}

function share_to_facebook(){
    location.href='http://www.facebook.com/share.php?u='+encodeURI(location.href);
}
function share_to_line(){
    window.open('https://social-plugins.line.me/lineit/share?url='+encodeURI(location.href), '_blank');
}
function share_to_twitter(text,params){
    window.open('https://twitter.com/share?url='+encodeURI(location.href)+"&text="+encodeURI(text)+params, '_blank');
}