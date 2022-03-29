class MyClass {
    constructor(name) {
        console.log("I am ready! My name is ", name)
    }
}

let my_class = new MyClass("Name!!")
;
if (my_class instanceof MyClass) (() => {
    console.log("It works!!!")
})();