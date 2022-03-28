let value = "abc";
if (new RegExp("[a-zA-Z0-9]+", "").test(value)) (() => {
    console.log("It works!!")
})(); else (() => {
    console.log("It does not work :(")
})();